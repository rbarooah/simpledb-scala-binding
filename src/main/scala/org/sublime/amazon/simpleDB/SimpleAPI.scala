package org.sublime.amazon.simpleDB {
	import scala.xml.PrettyPrinter
	
	class SimpleConnection (id:String, key:String) 
		extends Connection(id, key)
	{			
		class Item(val domain:Domain, val name:String) {
			/**
			 * Read all of the attributes from this item.
			 */
			def attributes = (new GetAttributesRequest(domain.name, name, Set()))
				.response.result.attributes
			
			/**
			 * Read a selection of attributes from this item
			 */
			def attributes (attributes:Set[String]) = 
				(new GetAttributesRequest(domain.name, name, Set())).response.result.attributes
				
			/**
			 * Read a single attribute from this item.
			 */
			def attribute (attributeName:String) =
				(new GetAttributesRequest(domain.name, name, Set(attributeName)))
					.response.result.attributes(attributeName)
			
			def putAttribute (pair :(String, String), replace:Boolean) = {
				(new PutAttributesRequest(domain.name, name, 
						Map(pair._1 -> (pair._2 -> replace))
					)
				).response.metadata				
			}
					
			/**
			 * Add a single attribute to this item.
			 */
			def += (pair:(String, String)) = putAttribute(pair, false)

			/**
			 * Replace a single attribute in this item.
			 */
			def set (pair:(String,String)) = putAttribute(pair, true)
			
			/** 
			 * Delete all of the attributes in this item.
			 */
			def clear = {
				(new DeleteAttributesRequest(domain.name, name, Map()).response.metadata)
			}
			
			/**
			 * Delete a single attribute value pair in this item.
			 */
			def -= (pair :(String, String)) = {
				(new DeleteAttributesRequest(domain.name, name, Map(pair._1 -> Set(pair._2))))
					.response.metadata
			}		
			
			/**
			 * Delete a single attribute in this item.
			 */
			def -= (name:String) = {
				(new DeleteAttributesRequest(domain.name, name, Map(name -> Set())))
					.response.metadata
			}
		}
		
		class Domain(val name:String) {
			def metadata = (new DomainMetadataRequest(name)).response.result
			def query (expression:String) = 
				(new QueryRequest(name, expression)).response.result.itemNames map (
					item(_)
				)					
					
			def delete = (new DeleteDomainRequest(name)).response.metadata
			def create = (new CreateDomainRequest(name)).response.metadata
			def item (name:String) = new Item(this, name)
			
			override def toString = name			
		}
		
		def domain (name:String) = new Domain(name)
					    
        def domains :Stream[Domain] = {
            var response:ListDomainsResponse = ListDomainsRequest.start.response
            var names = response.result.domainNames.toList           
            def take = names match {
                case head :: tail => {
                    names = tail
                    head
                }
                case List() => throw new RuntimeException("can't take from empty list")
            }
            
            def start : Stream[Domain] = {            
                if (names.isEmpty) Stream.empty
                else Stream.cons(new Domain(take), next)
            }
            
            def next : Stream[Domain] = {
                if (names.isEmpty) {
                    ListDomainsRequest.next(response) match {
                        case None => Stream.empty
                        case Some(request) =>
                            response = request.response
                            names = response.result.domainNames.toList
                            start
                    }                    
                } else Stream.cons(new Domain(take), next)
            }
                        
            start
        }
			    
		//// Simple test methods.
		
		def listDomains {
			Console.println(ListDomainsRequest.start.response.result)
		}
		
		def createDomain (name:String) {
			Console.println((new CreateDomainRequest(name)).response.metadata)
		}
		
		def deleteDomain (name:String) {
			Console.println((new DeleteDomainRequest(name)).response.metadata)
		}
		
		def domainMetadata (name:String) {
			Console.println((new DomainMetadataRequest(name)).response.result)
		}
		
		def putAttributes (domain:String, item:String, attributes:Map[String ,(String, Boolean)]) {
		    Console.println((new PutAttributesRequest(domain, item, attributes)).response.metadata)
		}
		
		def deleteAttributes (domain:String, item:String, attributes:Map[String, Set[String]]) {
		    Console.println((new DeleteAttributesRequest(domain, item, attributes)).response.metadata)
		}
		
		def getAttributes (domain:String, item:String, attributes:Set[String]) {
		    Console.println((new GetAttributesRequest(domain, item, attributes)).response.result)
		}
		
		def query (domain:String, query:String) {
		    Console.println((new QueryRequest(domain, query)).response.result)
		}
		
		def queryWithAttributes (domain:String, query:String, attributes:Set[String]) {
		    Console.println((new QueryWithAttributesRequest(domain, query, attributes)).response.result)
		}
	}
}