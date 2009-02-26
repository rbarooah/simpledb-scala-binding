import org.sublime.Attributes._
import org.sublime.Conversions._

package org.sublime.amazon.simpleDB {
    import api.{Domain, ItemSnapshot, SimpleAPI}
    import Quoting._
    
    object Select {
        
        implicit def toSelectAttribute[T] (a:Attribute[T]) :SelectAttribute [T] = 
            new SelectAttribute(a)

        trait FromExpression {
            def queryString:String
        }

        trait LimitableExpression extends FromExpression {
            def limit (n:int) = LimitedExpression(this, n)
        }

        case class LimitedExpression(e:LimitableExpression, n:int) extends FromExpression {
            def queryString = e.queryString + " limit " + n
        }

        trait SortedExpression extends LimitableExpression

        case class DescendingOrder [T] (e:Expression, a:Attribute[T]) extends SortedExpression
        {
            def queryString = e.queryString + " order by " + a.name + " desc"
        }
        
        case class AscendingOrder [T] (e:Expression, a:Attribute[T]) extends SortedExpression
        {
            def asc = this
            def desc = DescendingOrder(e, a)
            def queryString = e.queryString + " order by " + a.name + " asc"
        }

        trait Expression extends LimitableExpression
        {
            def order_by [T] (a:Attribute[T])  = AscendingOrder(this, a)            
            def queryString:String
        }   
        
        case object EmptyExpression extends Expression
        {
            def queryString = ""
        }
        
        case class Intersection (lhs:SelectExpression, rhs:SelectExpression) 
            extends SelectExpression
        {
            def queryString = lhs.queryString + " intersection " + rhs.queryString
        }
            
        case class Or (lhs:SelectExpression, rhs:SelectExpression) 
            extends SelectExpression
        {
            def queryString = lhs.queryString + " or " + rhs.queryString
        }
            
        case class And (lhs:SelectExpression, rhs:SelectExpression) 
            extends SelectExpression
        {
            def queryString = lhs.queryString + " and " + rhs.queryString
        }

        trait SelectExpression extends Expression {
            def intersection (e:SelectExpression) = Intersection(this, e)
            def or (e:SelectExpression) = Or(this, e)
            def and (e:SelectExpression) = And(this, e)
        }

        case class Not(e:SelectExpression) extends Expression
        {
            def queryString = "not " +e.queryString
        }

        def not (e:SelectExpression) = Not(e)

        trait Comparison extends SelectExpression
        
        case class BasicComparison [T] (op:String, c:Comparable[T], value:T) extends Comparison
        {
            def queryString = c.name + " " + op + " " + quote(c.conversion(value))
        }

        case class StringComparison [T] (op:String, c:Comparable[T], value:String)
            extends Comparison
        {
            def queryString = c.name + " " + op + " " + quote(value)
        }        

        case class Between [T] (c:Comparable[T], start:T, finish:T) extends Comparison
        {
            def queryString = 
                c.name + " between " + quote(c.conversion(start)) + " and " + 
                quote(c.conversion(finish))
        }

        case class BetweenLHS [T] (c:Comparable[T], start:T) {
            def and (finish:T) = Between(c, start,finish)
        }
        
        case class In [T] (c:Comparable[T], values:T*) extends Comparison {
            def terms = values map (t => quote(c.conversion(t))) mkString ", "
            def queryString = "in(" + terms + ")"
        }
        
        case class Unary [T] (op:String, c:Comparable[T]) extends Comparison {
            def queryString = c.name + " " + op
        }
        
        trait Comparable [T] {
            // requires
            def conversion:Conversion[T]
            def name:String
            
            // provides
            def comparison (op:String, value:T) = BasicComparison(op, this, value)
            def is (value:T) = comparison("=", value)
            def is_not (value:T) = comparison("!=", value)
            def > (value:T) = comparison(">", value)
            def >= (value:T) = comparison(">=", value)
            def < (value:T) = comparison("<", value)
            def <= (value:T) = comparison("<=", value)
            def like (value:String) = StringComparison("like", this, value)
            def not_like (value:String) = StringComparison("not like", this, value)
            def between (value:T) = BetweenLHS(this, value)
            def in (value:T*) = In[T](this, value:_*)
            def is_null = Unary("is null", this)
            def is_not_null = Unary("is not null", this)            
        }
        
        class SelectAttribute [T] (a:Attribute[T]) extends Comparable[T]
        {
            def conversion = a.conversion
            def name = a.name
        }

        class Every [T] (a:Attribute[T]) extends Comparable[T]
        {
            def conversion = a.conversion
            def name = "every("+a.name+")"
        }

        def every [T] (a:Attribute[T]) = new Every(a)

        implicit def toSelectableDomain (d:Domain) :SelectableDomain = new SelectableDomain(d)     
    
        /// Building elements of the query syntax
        private[Select] def names (attributes:NamedAttribute*) = 
            "(" + ((attributes map (a => a.name)) mkString ", ") + ")"
            
        private[Select] def whereClause (e:FromExpression) = "where "+e.queryString
            
        private[Select] def from (d:Domain) = "from "+d.name+" "
     
        class SelectableDomain (val d:Domain) {
            
            val all = "* "            
                        
            def apply (a:NamedAttribute*) (e:FromExpression) :Stream[ItemSnapshot] =
                d.api.select(names(a:_*) + from(d) + whereClause(e), d)      
            
            def apply (e:FromExpression) :Stream[ItemSnapshot] = where(e)
            
            def where (e:FromExpression) :Stream[ItemSnapshot] =
                d.api.select(all + from(d) + whereClause(e), d)
                            
            def count (e:Expression) :int = new CountSource(d.api, d).where(e)
            
            def count :int = count(EmptyExpression)
        }
                 
        class SourceSelection (attributes:AttributeSelection, d:Domain) {
            def where (e:FromExpression) :Stream[ItemSnapshot] = {
                d.api.select(attributes.queryString + from(d) + whereClause(e), d)
            }
        }
         
        class CountSource (api:SimpleAPI, d:Domain) {
            private val countValue = attribute("Count", PositiveInt)
            
            val toCount = "count(*) "
            
            private def values (whereClause:String) =  
                d.api.select(toCount + from(d) + whereClause, d) flatMap
                    {countValue(_)}
            
            def where (e:Expression) :int = 
                (0/: values(
                    e match {
                        case EmptyExpression => ""
                        case e:Expression => whereClause(e)
                    })) (_ + _)
        }
                  
        class AttributeSelection (val api:SimpleAPI, val queryString:String)
        {
            def from (d:Domain) = new SourceSelection(this, d)
            def from (name:String) = new SourceSelection(this, api.domain(name))
        }

        class CountSelection (val api:SimpleAPI) {
            def from (d:Domain) = new CountSource(api, d)
            def from (name:String) = new CountSource(api, api.domain(name))
        }
    }
}