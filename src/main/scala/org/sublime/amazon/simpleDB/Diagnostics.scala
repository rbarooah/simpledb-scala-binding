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

        def putAttributes (domain:String, item:String, 
            attributes:Map[String ,(Set[String], Boolean)]) {
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