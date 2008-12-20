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
	
	trait SimpleDBRequest {
		def action:String
		def awsAccessKeyId:String
		def timeStamp:String
		def version:String = "2007-11-07"
		
		def parameters = Map(
			"Action" -> action, 
			"AWSAccessKeyId" -> awsAccessKeyId,
			"Timestamp" -> timeStamp,
			"Version" -> version
		) ++ specificParameters
		
		def specificParameters:Map[String, String]
	}
	
	trait CreateDomain extends SimpleDBRequest {
		def action = "CreateDomain"
		def domainName:String		
		def specificParameters = Map("DomainName" -> domainName)
	}
	
	trait DeleteDomain extends SimpleDBRequest {
		def action = "DeleteDomain"
		def domainName:String		
		def specificParameters = Map("DomainName" -> domainName)
	}
	
	trait ListDomains extends SimpleDBRequest {
	    import Attributes._
	    
		def action = "ListDomains"
		def maxNumberOfDomains:Option[int]
		def nextToken:Option[String]
		def specificParameters = 
			optional("MaxNumberOfDomains", maxNumberOfDomains) ++
			optional("NextToken", nextToken)					
	}
	
	trait DomainMetadata extends SimpleDBRequest {
		def action = "DomainMetadata"
		def domainName:String		
		def specificParameters = Map("DomainName" -> domainName)
	}
	
	trait PutAttributes extends SimpleDBRequest {
	    import Attributes._
	    
		def action = "PutAttributes"
		def itemName:String
		def attributes:Map[String, (Set[String] , Boolean)]
		def domainName:String		
		def specificParameters = replacableAttributes ++ 
			Map("DomainName" -> domainName, "ItemName"->itemName) 
		
		def replacableAttributes : Map[String,String] = {
            def flattened = attributes flatMap (e => e match {
              case (name, (value, replace)) => for (each <- value) yield (name, each, replace)
            }) 
            
            def params :List[Map[String, String]] = 
                (flattened.toList zipWithIndex) map (z => z match {
                    case ((name, value, replace), pos) => 
                        param("Name", pos, name) ++ param ("Value", pos, value) ++
                            (if (replace) param("Replace", pos, "True")
                            else Map.empty)
                })
                
            (Map[String, String]() /: params) (_ ++ _)
		}		
	}
	
	trait DeleteAttributes extends SimpleDBRequest {
	    import Attributes._
	    
		def action = "DeleteAttributes"
		def attributes:Map[String, Set[String]]
		def domainName:String
		def itemName:String
		def specificParameters = Map("DomainName" -> domainName, "ItemName" -> itemName) ++ 
		    attributeNameValues	
		
		def attributeNameValues :Map[String, String] = {
			
			def flattened = attributes flatMap { e => e match {
			    case (name, values) => 
			        (if (values isEmpty) List((name, None))
			        else for (value <- values) yield (name, Some(value)))
			    }
			}
			
			def numbered = flattened.toList zipWithIndex
			
			def params = numbered map ( e => e match {
			    case ((name, value), pos) => 
			        param ("Name", pos, name) ++
			        (value match {
			            case None => param("Name", pos, name)
			            case Some(value) => param("Value", pos, value)
			        })
			})
			
			(Map[String,String]() /: params) (_ ++ _)
		}
	}
	
	trait GetAttributes extends SimpleDBRequest {
		import Attributes._
		def action = "GetAttributes"
		def itemName:String
		def domainName:String
		def attributes:Set[String]
		def specificParameters = attributeNames(attributes) ++
			Map("ItemName"->itemName, "DomainName" -> domainName)	
	}
	
	trait Query extends SimpleDBRequest {
	    import Attributes._
	    
		def action = "Query"
		def maxNumberOfItems:Option[int]
		def nextToken:Option[String]
		def queryExpression:Option[String]
		def domainName:String
		
		def specificParameters = Map("DomainName" -> domainName) ++ 
			optional("QueryExpression", queryExpression) ++
			optional("MaxNumberOfItems", maxNumberOfItems) ++
			optional("NextToken", nextToken)			
	}
		
	trait QueryWithAttributes extends SimpleDBRequest {
		import Attributes._
		
		def action = "QueryWithAttributes"
		def attributes:Set[String]
		def maxNumberOfItems:Option[int]
		def nextToken:Option[String]
		def queryExpression:Option[String]
		def domainName:String		
		
		def specificParameters = Map[String, String] ("DomainName" -> domainName) ++
            attributeNames(attributes) ++
			optional("QueryExpression", queryExpression) ++			
			optional("MaxNumberOfItems", maxNumberOfItems) ++
			optional("NextToken", nextToken)			
	}	
	
	trait Select extends SimpleDBRequest {
	    import Attributes._
	    
	    def action = "Select"
	    def maxNumberOfItems:Option[int]
	    def nextToken:Option[String]
	    def selectExpression:String
	    
	    def specificParameters = Map[String, String] ("SelectExpression" -> selectExpression) ++
	        optional("MaxNumberOfItems", maxNumberOfItems) ++
	        optional("NextToken", nextToken)
	}
	
	object Attributes {
	    def param (kind:String, pos:int, value:String) = Map("Attribute."+pos+"."+kind -> value)        
	    
		def attributeNames (names:Set[String]) :Map[String, String] = {
			import scala.collection.immutable.HashMap;		
			var coded :Map[String, String] = new HashMap[String, String]()
			var pos = 0;
			for (name <- names) {
				coded = coded + ("AttributeName."+pos -> name)
			}			
			coded
		}
		
		def optional [T] (name:String, value:Option[T]) :Map[String, String] =
			value match {
			    case Some(v) => Map(name->v.toString)
			    case None => Map.empty
			}						
	}
}