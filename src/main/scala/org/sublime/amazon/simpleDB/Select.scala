package org.sublime.amazon.simpleDB {
    import Query._
    
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