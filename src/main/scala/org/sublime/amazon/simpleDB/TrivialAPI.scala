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
		
		def makeRequest (request:SimpleDBRequest) :Elem = {
			val method = 
				new PostMethod(url + 
					QueryParameters(signer.sign(request.parameters)))
			client.executeMethod(method)
			val xml = XML.load(method.getResponseBodyAsStream())
			method.releaseConnection
			xml
		}
		
		val printer = new PrettyPrinter(80, 2)
		
		def diagnose (request:SimpleDBRequest) {
			Console.println(printer.format(makeRequest(request)))
		}		
		
		trait Basics {
			def timeStamp = now()
			def awsAccessKeyId = id
		}
		
		class ListDomainsRequest extends ListDomains with Basics {
			val nextToken = None
			val maxNumberOfDomains = None
			
			def response = new ListDomainsResponse() (makeRequest(this))			
		}
		
		class CreateDomainRequest (val domainName:String) extends CreateDomain with Basics
		{
			def response = new CreateDomainResponse() (makeRequest(this))
		}
		
		class DeleteDomainRequest (val domainName:String) extends DeleteDomain with Basics
		{
			def response = new DeleteDomainResponse() (makeRequest(this)) 
		}
		
		class DomainMetadataRequest (val domainName:String) extends DomainMetadata with Basics
		{
			def response = new DomainMetadataResponse() (makeRequest(this))
		}
	}	
}