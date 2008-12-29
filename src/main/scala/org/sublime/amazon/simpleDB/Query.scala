package org.sublime.amazon.simpleDB {
    
    object Query {        

        def quote (value:String) = "'" + ((value map { c => c match {
            case '"' => "\"\""
            case '`' => "``"
            case '\'' => "''"
            case a:Char => a 
        }}) mkString) + "'"        
        
        abstract case class Expression {
            //def query :String
        }

        trait Combinable extends Expression
        {
            def intersection (other:Combinable) = Combination("intersection", this, other)
            def union (other:Combinable) = Combination("union", this, other)
        }

        case class Combination(operation:String, lhs:Expression, rhs:Expression) 
            extends Expression  with Combinable
        {        
            override def toString = lhs + " "+operation+" "+rhs
        }    
        
        trait Negatable extends Expression

        case class Negation(target:Negatable) extends Expression with Combinable
        {
            override def toString = "not " + target;
        }
        
        case class DescendingSort(target:Sortable, name:String) extends Expression with Negatable
        {
            override def toString = "[" + target.component + "] sort " + quote(name) + " desc"
        }

        case class AscendingSort(target:Sortable, name:String) extends Expression with Negatable
        {
            def desc = DescendingSort(target, name)
            override def toString = "[" + target.component + "] sort " + quote(name) + " asc"
        }

        trait Sortable extends Predicate {
            def sort [T] (attribute:Attribute[T]) :AscendingSort 
                = AscendingSort(this, attribute.name)                        
        }

        abstract case class Predicate extends Expression with Negatable
        {
            def and (other:Predicate) = Conjunction("and", this, other)
            def or (other:Predicate) = Conjunction("and", this, other)
            def intersection (other:Predicate) = Combination("intersection", this, other)
            def union (other:Predicate) = Combination("union", this, other)
            def unary_! = Negation(this) 

            def component :String
            override def toString = "[" + component + "]"
        }
    
        case class Conjunction (operator:String, lhs:Predicate, rhs:Predicate)
            extends Predicate with Sortable
        {
            def component = lhs.component + " " + operator + " " + rhs.component
        }

        case class Comparison [T] (operator:String, attribute:Attribute[T], value:T)
            extends Predicate with Sortable
        {
            def component = {
                val (name, converted) = attribute(value) 
                quote (name) + " " + operator + " " + quote (converted)
            }
        }
        
        case class NamedAttribute (name:String)

        case class Attribute [T] (override val name:String, conversion:Conversion[T]) extends
            NamedAttribute(name)
        {   
            def apply (value:T) = (name -> conversion(value))
            
            import scala.collection.Map
            def apply (result:Map[String,Set[String]]) : List[T] = 
                if (! result.contains(name)) List[T]()
                else (result(name) flatMap ( raw => raw match {
                    case conversion(value) => List[T](value)
                    case _ => List[T]()
                } )).toList
        
            private def comparison (op:String, value:T) = Comparison(op, this, value)
        
            def eq (value:T) = comparison("=", value)
            def ne (value:T) = comparison("!=", value)
            def > (value:T) = comparison(">", value)
            def < (value:T) = comparison("<", value)
            def >= (value:T) = comparison(">=", value)
            def <= (value:T) = comparison("<=", value)
            def starts_with (value:T) = comparison("starts-with", value)
            def does_not_start_with (value:T) = comparison("does_not_start_with", value)
        }
        
        def attribute (name:String) = Attribute(name, Conversions.PassThrough)
        def attribute [T] (name:String, conversion:Conversion[T]) = Attribute[T](name, conversion)
    }
}