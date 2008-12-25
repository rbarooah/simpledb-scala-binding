package org.sublime.amazon.simpleDB {
    
    object Query {
        abstract case class Expression {
            //def query :String
        }

        trait Combinable extends Expression
        {
            def intersection (other:Combinable) = Combination("intersection", this, other)
            def union (other:Combinable) = Combination("union", this, other)
        }

        case class Combination(operation:String, lhs:Expression, rhs:Expression) 
            extends Expression with Sortable with Combinable
        {        
            override def toString = lhs + " "+operation+" "+rhs
        }    

        case class Negation(target:Predicate) extends Expression with Combinable
        {
            override def toString = "not " + target;
        }

        case class DescendingSort(target:Expression, name:String) extends Expression
        {
            override def toString = target.toString + " order by " + name + " desc"
        }

        case class AscendingSort(target:Expression, name:String) extends Expression
        {
            def desc = DescendingSort(target, name)
            override def toString = target.toString + " order by " + name + " asc"
        }

        trait Sortable extends Expression {
            def order_by [T] (attribute:Attribute[T]) :AscendingSort 
                = AscendingSort(this, attribute.name)        
        }

        abstract case class Predicate extends Expression with Sortable
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
            extends Predicate
        {
            def component = lhs.component + " " + operator + " " + rhs.component
        }

        case class Comparison [T] (operator:String, attribute:Attribute[T], value:T)
            extends Predicate
        {
            def quote (value:String) = "'" + ((value map { c => c match {
                case '"' => "\"\""
                case '`' => "``"
                case '\'' => "''"
                case a:Char => a 
            }}) mkString) + "'"

            def component = {
                val (name, converted) = attribute(value) 
                quote (name) + " " + operator + " " + quote (converted)
            }
        }    

        case class Attribute [T] (name:String, conversion:Conversion[T])
        {   
            def apply (value:T) = (name -> conversion(value))
            def apply (result:Map[String,Set[String]]) = 
                if (! result.contains(name)) Set[T]()
                else result(name) flatMap ( raw => raw match {
                    case conversion(value) => List(value)
                    case _ => List()
                } )
        
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
        
        def attribute (name:String) = Attribute(name, Unconverted)
        def attribute [T] (name:String, conversion:Conversion[T]) = Attribute(name, conversion)
    }
}