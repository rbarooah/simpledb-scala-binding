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
		def action = "PutAttributes"
		def itemName:String
		def attributes:Map[String, (String, Boolean)]
		def domainName:String		
		def specificParameters = attributeNames ++ 
			Map("DomainName" -> domainName, "ItemName"->itemName) 
		
		def attributeNames :Map[String,String] = {
			import scala.collection.immutable.HashMap;
			var coded:Map[String, String] = new HashMap[String, String]()
			var pos = 0;
			
			for (name <- attributes.keys) {
				coded = coded + ("Attribute."+pos+".Name" -> name)
				coded = coded + ("Attribute."+pos+".Value" -> attributes(name)._1)
				if (attributes(name)._2) {
					coded = coded + ("Attribute."+pos+".Replace" -> "true")
				}
				pos = pos + 1;
			}
			coded
		}
	}
	
	trait DeleteAttributes extends SimpleDBRequest {
		def action = "DeleteAttributes"
		def attributes:Map[String, Set[String]]
		def domainName:String		
		def itemName:String
		def specificParameters = Map("DomainName" -> domainName, "ItemName" -> itemName) ++ 
		    attributeNames	
		
		def attributeNames :Map[String, String] = {
			import scala.collection.immutable.HashMap;		
			var coded :Map[String, String] = new HashMap[String, String]()
			var pos = 0;
			
			def addName (name:String) {
				coded = coded + ("Attribute."+pos+".Name" -> name)
				pos = pos + 1
			}
			
			def addPair (name:String, value:String) {
				coded = coded + ("Attribute."+pos+".Name" -> name)
				coded = coded + ("Attribute."+pos+".Value" -> value)
				pos = pos + 1
			}
			
			for (name <- attributes.keys) {
				val set = attributes(name)
				if (set.size <= 1) addName(name) 
					else for (value <- set) addPair(name, value)
			}

			coded
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
	    def domainName:String
	    
	    def specificParameters = Map[String, String] ("DomainName" => domainName,
	        "SelectExpression" -> selectExpression) ++
	        optional("MaxNumberOfItems", maxNumberOfItems) ++
	        optional("NextToken", nextToken)
	}
	
	object Attributes {
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