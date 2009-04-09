package org.sublime.amazon.simpleDB {
    
    object Quoting {
     
        /**
         * Generic quoting function
         */
        def quote (string:String, quote:Char, rules: Char => Any) =
            quote + ((string map {rules}) mkString) + quote

        /**
         * Quote a value.
         */
        def quoteValue (value:String) = quote(value, '\'', valueRules)
        
        /**
         * Quote an attribute name.
         */
        def quoteName (name:String) = quote(name, '`', attributeRules)
     
        /**
         * Rules for quoting attribute names.
         */
        def attributeRules (a:Char) = a match {
            case '`' => "``"
            case a:Char => a
        }
        
        /**
         * Rules for quoting values.
         */
        def valueRules (a:Char) = a match {
            case '"' => "\"\""
            case '`' => "``"
            case '\'' => "''"
            case a:Char => a
        }
    }    
}