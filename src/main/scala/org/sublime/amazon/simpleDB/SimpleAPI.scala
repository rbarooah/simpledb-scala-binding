package org.sublime.amazon.simpleDB {
	import scala.xml.PrettyPrinter
	
	class SimpleConnection (id:String, key:String) 
		extends Connection(id, key)
	{						
		class Item(val domain:Domain, val name:String) {
			
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
		
		def domains = (new ListDomainsRequest()).response.result.domainNames map (
				new Domain(_)
			)
			    
		//// Simple test methods.
		
		def listDomains {
			Console.println((new ListDomainsRequest()).response.result)
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