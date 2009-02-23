package org.sublime.amazon.simpleDB {
    
    object Quoting {
     
        def quote (value:String) = "'" + ((value map { c => c match {
            case '"' => "\"\""
            case '`' => "``"
            case '\'' => "''"
            case a:Char => a 
        }}) mkString) + "'"        
   
    }    
}