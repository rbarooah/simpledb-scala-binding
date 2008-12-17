package org.sublime.amazon.simpleDB {

    trait Diagnostics extends Concrete
    {
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
            Console.println((QueryRequest.start(domain, Some(query))).response.result)
        }

        def queryWithAttributes (domain:String, query:String, attributes:Set[String]) {
            Console.println((new QueryWithAttributesRequest(domain, Some(query), attributes)).response.result)
        }
    }
}