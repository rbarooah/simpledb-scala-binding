import org.sublime.Attributes._

package org.sublime.amazon.simpleDB {
    
    object Select {
     
        trait Source {
            
//            def where (selectExpression:
            
        }
     
        trait SelectDSL {

            def select (attributes:Attribute[Any]*) :Source
            
        }

        trait SelectableDomain {

            def where ()

        }  
    }
}