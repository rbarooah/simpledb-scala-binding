import org.sublime.Attributes._
import org.sublime.Conversions._

package org.sublime.amazon.simpleDB {
    import api.{Domain, ItemSnapshot}
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
            def queryString = e.queryString + " order by " + quote(a.name) + " desc"
        }
        
        case class AscendingOrder [T] (e:Expression, a:Attribute[T]) extends SortedExpression
        {
            def asc = this
            def desc = DescendingOrder(e, a)
            def queryString = e.queryString + " order by " + quote(a.name) + " asc"
        }

        trait Expression extends LimitableExpression
        {
            def order_by [T] (a:Attribute[T])  = AscendingOrder(this, a)            
            def queryString:String
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
            def queryString = quote(c.name) + " " + op + " " + quote(c.conversion(value))
        }

        case class StringComparison [T] (op:String, c:Comparable[T], value:String)
            extends Comparison
        {
            def queryString = quote(c.name) + " " + op + " " + quote(value)
        }        

        case class Between [T] (c:Comparable[T], start:T, finish:T) extends Comparison
        {
            def queryString = 
                quote(c.name) + " between " + quote(c.conversion(start)) + " and " + 
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
            def queryString = quote(c.name) + " " + op
        }
        
        trait Comparable [T] {
            // requires
            def conversion:Conversion[T]
            def name:String
            
            // provides
            def comparison (op:String, value:T) = BasicComparison(op, this, value)
            def eq (value:T) = comparison("=", value)
            def ne (value:T) = comparison("!=", value)
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
            def name = "every("+quote(a.name)+")"
        }

        def every [T] (a:Attribute[T]) = new Every(a)

        implicit def toSelectableDomain (d:Domain) = new SelectableDomain(d)     
     
        class SelectableDomain(val d:Domain) {
            
            val all = "* "
            
            val toCount = "count(*) "
            
            private def whereClause (e:FromExpression) = "where "+e.queryString
            
            private def from (d:Domain) = "from "+d.name
            
            private def names (attributes:Attribute[Any]*) = 
                (attributes map (a => quote(a.name))) mkString ", "
            
            def apply (a:Attribute[Any]*) (e:FromExpression) :Stream[ItemSnapshot] =
                d.api.select(names(a:_*) + from(d) + whereClause(e), d)      
            
            def apply (e:FromExpression) :Stream[ItemSnapshot] = where(e)
            
            def where (e:FromExpression) :Stream[ItemSnapshot] =
                d.api.select(all + from(d) + whereClause(e), d)
                
            private val countValue = attribute("Count", PositiveInt)
                                
            def count (e:Expression) :int = {
                def values = d.api.select(toCount + from(d) + whereClause(e), d) flatMap
                    {countValue(_)}
                    
                (0 /: values) (_ + _)
            }
        }        
    }
}