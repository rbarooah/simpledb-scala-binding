package org.sublime.amazon.simpleDB {
	import scala.xml._
	
	trait Concrete { 
	    concrete =>
		
        def makeRequest (request:SimpleDBRequest) :Elem
		
		def awsAccessKeyId :String
		
		def now () :String = dateFormat.format(new java.util.Date())		
		
		import java.text.SimpleDateFormat
		val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")		
						
		trait Basics {
			def timeStamp = now()
			def awsAccessKeyId = concrete.awsAccessKeyId
		}
		
		class ListDomainsRequest (val nextToken:Option[String], val maxNumberOfDomains:Option[int]) 
			extends ListDomains with Basics {
			
			def response = new ListDomainsResponse() (makeRequest(this))			
		}
		
		object ListDomainsRequest {
			def start = new ListDomainsRequest(None, Some(2))
			def start (maxNumberOfDomains:int) = new ListDomainsRequest(None, Some(maxNumberOfDomains))
			def next (response:ListDomainsResponse) :Option[ListDomainsRequest] = 
			    response.result.nextToken match {			    
			        case None => None
			        case Some(token) => Some(new ListDomainsRequest(Some(token), Some(2)))
		        }
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
		
		class QueryRequest (val domainName:String, val queryExpression:Option[String],
		    val nextToken:Option[String], val maxNumberOfItems:Option[int]) 
		    extends Query with Basics
		{
		    def response = new QueryResponse() (makeRequest(this))
	    }

        object QueryRequest {
            def start (domainName:String, queryExpression:Option[String]) =
                new QueryRequest(domainName, queryExpression, None, None)
            def next (req:QueryRequest, res:QueryResponse) :Option[QueryRequest] =
                res.result.nextToken match {
                    case None => None
                    case Some(token) =>
                        Some(new QueryRequest(req.domainName, 
                                req.queryExpression, Some(token), None))
                }
        }
	    
	    class QueryWithAttributesRequest (val domainName:String, 
	        val queryExpression:Option[String],
	        val attributes:Set[String]) extends QueryWithAttributes with Basics
	    {
		    val nextToken:Option[String] = None
		    val maxNumberOfItems = None
		    
	        def response = new QueryWithAttributesResponse() (makeRequest(this))
	    }	    
	    
	    object QueryWithAttributesRequest
	    {
	        def start (domainName:String, queryExpression:Option[String], attributes:Set[String]) =
	            new QueryWithAttributesRequest (domainName, queryExpression, attributes)
	        
	        def next (req:QueryWithAttributesRequest, res:QueryWithAttributesResponse)
	            :Option[QueryWithAttributesRequest] =
    	            res.result.nextToken match {
    	                case None => None
    	                case Some(token) =>
    	                    Some(new QueryWithAttributesRequest(
    	                        req.domainName, 
    	                        req.queryExpression,
    	                        req.attributes) {
    	                                override val nextToken:Option[String] = Some(token)
    	                            }
    	                    )
    	            }
	    }
	}	
}