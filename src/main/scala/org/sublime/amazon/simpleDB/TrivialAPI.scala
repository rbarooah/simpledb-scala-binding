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
		def trace = false
		
		def now () :String = dateFormat.format(new java.util.Date())
		
		import java.text.SimpleDateFormat
		val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
		
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
		
		class PutAttributesRequest (val domainName:String, 
		    val itemName:String, 
		    val attributes:Map[String, (String, Boolean)]) extends PutAttributes with Basics
		{
		    def response = new PutAttributesResponse() (makeRequest(this))
		}
		
		class DeleteAttributesRequest (val domainName:String, 
		    val itemName:String,
		    val attributes:Map[String, Set[String]]) extends DeleteAttributes with Basics
		{
		    def response = new DeleteAttributesResponse() (makeRequest(this))
		}
		
		class GetAttributesRequest (val domainName:String,
		    val itemName:String,
		    val attributes:Set[String]) extends GetAttributes with Basics
		{
		    def response = new GetAttributesResponse() (makeRequest(this))
		}
		
		class QueryRequest (val domainName:String, val queryExpression:String) 
		    extends Query with Basics
		{
		    val nextToken = None
		    val maxNumberOfItems = None
		    
		    def response = new QueryResponse() (makeRequest(this))
	    }
	    
	    class QueryWithAttributesRequest (val domainName:String, 
	        val queryExpression:String,
	        val attributes:Set[String]) extends QueryWithAttributes with Basics
	    {
		    val nextToken = None
		    val maxNumberOfItems = None
		    
	        def response = new QueryWithAttributesResponse() (makeRequest(this))
	    }	    
	}	
}