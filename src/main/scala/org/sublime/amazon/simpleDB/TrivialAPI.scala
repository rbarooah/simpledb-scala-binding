package org.sublime.amazon.simpleDB {
	import scala.xml._
	
	object Service {
		val url = "https://sdb.amazonaws.com"
	}
	
	class Connection (val id:String, secretKey:String) {
		import Service._
		
		import org.apache.commons.httpclient.HttpClient
		import org.apache.commons.httpclient.methods.{GetMethod, PostMethod}
		
		val signer = new Signer(secretKey)
		val client = new HttpClient()
		
		def timestamp () :String = dateFormat.format(new java.util.Date())
		
		import java.text.SimpleDateFormat
		val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
		
		def makeRequest (request:SimpleDBRequest) :NodeSeq = {
			val method = 
				new PostMethod(url + 
					QueryParameters(signer.sign(request.parameters)))
			client.executeMethod(method)
			val xml = XML.load(method.getResponseBodyAsStream())
			method.releaseConnection
			xml
		}
		
		def listDomainsRequest = new ListDomains() {
			val nextToken = None
			val maxNumberOfDomains = None
			val timeStamp = timestamp()
			val awsAccessKeyId = id
		}
	}	
}