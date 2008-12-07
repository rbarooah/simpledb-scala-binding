package org.sublime.amazon.simpleDB {
    
    class SimpleDBException(val code:String, val message:String, val boxUsage:Double)
        extends Exception(code+": "+message+" ("+boxUsage+"s)")
                    
}