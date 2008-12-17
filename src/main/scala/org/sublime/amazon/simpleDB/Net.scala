package org.sublime.amazon.simpleDB {
	import scala.xml._
	
	object Service {
		val url = "https://sdb.amazonaws.com"
	}
	
	class Connection (val awsAccessKeyId:String, secretKey:String) {
		import Service._
		
		import org.apache.commons.httpclient.HttpClient
		import org.apache.commons.httpclient.methods.{GetMethod, PostMethod}
		
		val signer = new Signer(secretKey)
		val client = new HttpClient()
		def trace = false
				
		def makeRequest (request:SimpleDBRequest) :Elem = {
		    if (trace) diagnose(request.parameters)
			val method = 
				new PostMethod(url + 
					QueryParameters(signer.sign(request.parameters)))
			client.executeMethod(method)
			val xml = XML.load(method.getResponseBodyAsStream())
			method.releaseConnection
			if (trace) diagnose(xml)
			xml match { 
			    case Error(code, message, boxUsage) => 
			        throw new SimpleDBException(code, message, boxUsage)
			    case _ => xml
		    }
		}
		
		val printer = new PrettyPrinter(80, 2)
		
		def diagnose (xml:Node) {
		    Console.println(printer.format(xml))
		}
		
		def diagnose (parameters:Map[String, String]) {
		    Console.println(
		        (parameters.keys map (k => k + ": "+parameters(k))) mkString "\n"
		    )
		}
		
		// Accounting for box usage
		private var totalBoxUsage:double = 0 
		private var lastBoxUsage:double = 0 
		
		def accountFor [T <: SimpleDBResponse] (response:T) :T = {
		    lastBoxUsage = response.metadata.boxUsage
		    totalBoxUsage = totalBoxUsage + lastBoxUsage
		    response
		}						
	}	
}