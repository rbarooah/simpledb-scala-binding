// Copyright 2008 Robin Barooah
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.sublime.amazon.simpleDB {
    import Request._
	import scala.xml._
	
	trait Concrete { 
	    concrete =>
		
        def makeSimpleDBRequest (request:SimpleDBRequest) :Elem
		
		def awsAccessKeyId :String
		
		def now () :String = dateFormat.format(new java.util.Date())		
		
		import java.text.SimpleDateFormat
		val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")		
						
		trait Basics {
			def timeStamp = now()
			def awsAccessKeyId = concrete.awsAccessKeyId
		}
		
		class ListDomainsRequest (val nextToken:Option[String], val maxNumberOfDomains:Option[Int]) 
			extends ListDomains with Basics {
			
			def response = new ListDomainsResponse() (makeSimpleDBRequest(this))			
		}
		
		object ListDomainsRequest {
			def start = new ListDomainsRequest(None, Some(2))
			def start (maxNumberOfDomains:Int) = new ListDomainsRequest(None, Some(maxNumberOfDomains))
			def next (response:ListDomainsResponse) :Option[ListDomainsRequest] = 
			    response.result.nextToken map 
			        (token => new ListDomainsRequest(Some(token), Some(2)))						    
		}
		
		class CreateDomainRequest (val domainName:String) extends CreateDomain with Basics
		{
			def response = new CreateDomainResponse() (makeSimpleDBRequest(this))
		}
		
		class DeleteDomainRequest (val domainName:String) extends DeleteDomain with Basics
		{
			def response = new DeleteDomainResponse() (makeSimpleDBRequest(this))
		}
		
		class DomainMetadataRequest (val domainName:String) extends DomainMetadata with Basics
		{
			def response = new DomainMetadataResponse() (makeSimpleDBRequest(this))
		}
		
		class PutAttributesRequest (val domainName:String, 
		    val itemName:String, 
		    val attributes:Map[String, (Set[String], Boolean)]) extends PutAttributes with Basics
		{
		    def response = new PutAttributesResponse() (makeSimpleDBRequest(this))
		}
		
		class BatchPutAttributesRequest (val domainName:String, 
		    val operations:List[AttributeOperation]) extends BatchPutAttributes with Basics
		{
		    def response = new BatchPutAttributesResponse() (makeSimpleDBRequest(this))
		}
		
		class DeleteAttributesRequest (val domainName:String, 
		    val itemName:String,
		    val attributes:Map[String, Set[String]]) extends DeleteAttributes with Basics
		{
		    def response = new DeleteAttributesResponse() (makeSimpleDBRequest(this))
		}
		
		class GetAttributesRequest (val domainName:String,
		    val itemName:String,
		    val attributes:Set[String]) extends GetAttributes with Basics
		{
		    def response = new GetAttributesResponse() (makeSimpleDBRequest(this))
		}
		
		class QueryRequest (val domainName:String, val queryExpression:Option[String],
		    val nextToken:Option[String], val maxNumberOfItems:Option[Int]) 
		    extends Query with Basics
		{
		    def response = new QueryResponse() (makeSimpleDBRequest(this))
	    }

        object QueryRequest {
            def start (domainName:String, queryExpression:Option[String]) =
                new QueryRequest(domainName, queryExpression, None, None)
            def next (req:QueryRequest, res:QueryResponse) :Option[QueryRequest] =
                res.result.nextToken map
                    (token =>
                        new QueryRequest(req.domainName, 
                            req.queryExpression, Some(token), None))
        }
	    
	    class QueryWithAttributesRequest (val domainName:String, 
	        val queryExpression:Option[String],
	        val attributes:Set[String]) extends QueryWithAttributes with Basics
	    {
		    val nextToken:Option[String] = None
		    val maxNumberOfItems = None
		    
	        def response = new QueryWithAttributesResponse() (makeSimpleDBRequest(this))
	    }	    
	    
	    object QueryWithAttributesRequest
	    {
	        def start (domainName:String, queryExpression:Option[String], attributes:Set[String]) =
	            new QueryWithAttributesRequest(domainName, queryExpression, attributes)
	        
	        def next (req:QueryWithAttributesRequest, res:QueryWithAttributesResponse)
	            :Option[QueryWithAttributesRequest] =
    	            res.result.nextToken map (
    	                token =>
    	                    new QueryWithAttributesRequest(
    	                        req.domainName, 
    	                        req.queryExpression,
    	                        req.attributes) {
    	                            override val nextToken:Option[String] = Some(token)
    	                        }
    	                    )
	    }
	    
	    class SelectRequest (val selectExpression:String, 
	        val nextToken:Option[String], val maxNumberOfItems:Option[Int]) 
	        extends Select with Basics 
	    {
	        def response = new SelectResponse() (makeSimpleDBRequest(this))
	    }
	    
	    object SelectRequest {
	        def start (selectExpression:String) =
	            new SelectRequest(selectExpression, None, None)
	            
	        def next (req:SelectRequest, res:SelectResponse) :Option[SelectRequest] =
	            res.result.nextToken map (
	                token =>
	                    new SelectRequest(req.selectExpression, 
	                        Some(token), None))	        
	    }	    
	}	
}
