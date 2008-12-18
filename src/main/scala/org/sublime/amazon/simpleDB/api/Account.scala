package org.sublime.amazon.simpleDB.api {
    import scala.xml._
    
    class SimpleDBAccount (val awsAccessKeyId:String, awsSecretKey:String) extends SimpleAPI
    {
        val connection = new Connection(awsAccessKeyId, awsSecretKey)
        
        def makeSimpleDBRequest (req:SimpleDBRequest) :Elem = connection.makeRequest(req)
    }
}