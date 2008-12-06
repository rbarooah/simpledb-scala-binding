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
		
		def now () :String = dateFormat.format(new java.util.Date())
		
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
		
		trait Basics {
			def timeStamp = now()
			def awsAccessKeyId = id
		}
		
		class ListDomainsRequest extends ListDomains() with Basics {
			val nextToken = None
			val maxNumberOfDomains = None
		}
		
		class CreateDomainRequest (val domainName:String) extends CreateDomain with Basics
	}	
}