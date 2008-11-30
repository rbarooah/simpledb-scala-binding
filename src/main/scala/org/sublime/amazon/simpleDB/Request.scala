package org.sublime.amazon.simpleDB {
	
	trait RequiredParameters {
		def action:String
		def awsAccessKeyId:String
		def domainName:String
		def timeStamp:String
		def version:String = "2007-11-07"
		
		def parameters = Map(
			"Action" -> action, 
			"AWSAccessKeyId" -> awsAccessKeyId,
			"DomainName" -> domainName,
			"Timestamp" -> timeStamp,
			"Version" -> version
		) ++ specificParameters
		
		def specificParameters:Map[String, String]
	}
	
	trait CreateDomain extends RequiredParameters {
		def action = "CreateDomain"
		def specificParameters = Map.empty
	}
	
	trait DeleteDomain extends RequiredParameters {
		def action = "DeleteDomain"
		def specificParameters = Map.empty
	}
	
	trait ListDomains extends RequiredParameters {
		def action = "ListDomains"
		def maxNumberOfDomains:Option[String]
		def nextToken:Option[String]
		def specificParameters = 
			optional("MaxNumberOfDomains", maxNumberOfDomains) ++
			optional("NextToken", nextToken)		
			
		def optional (name:String, value:Option[String]) :Map[String,String] =
			value match {
				case Some(v) => Map(name->v)
				case None => Map.empty
			}				
	}
	
	trait PutAttributes extends RequiredParameters {
		def action = "PutAttributes"
		def itemName:String
		def attributes:Map[String, (String, Boolean)]
		def specificParameters = attributeNames ++ Map("ItemName"->itemName) 
		
		def attributeNames :Map[String,String] = {
			import scala.collection.immutable.HashMap;
			var coded:Map[String, String] = new HashMap[String, String]()
			var pos = 0;
			
			for (name <- attributes.keys) {
				coded = coded + ("Attribute."+pos+"."+name -> attributes(name)._1)
				if (attributes(name)._2) {
					coded = coded + ("Attribute."+pos+".Replace" -> "true")
				}
				pos = pos + 1;
			}
			coded
		}
	}
	
	trait DeleteAttributes extends RequiredParameters {
		def action = "DeleteAttributes"
		def attributes:Map[String, Set[String]]
		def specificParameters = attributeNames
		
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
	
	trait GetAttributes extends RequiredParameters {
		import Attributes._
		def action = "GetAttributes"
		def itemName:String
		def attributes:Set[String]
		def specificParameters = attributeNames(attributes) ++
			Map("ItemName"->itemName)	
	}
	
	trait Query extends RequiredParameters {
		def action = "Query"
		def maxNumberOfItems:int
		def nextToken:String
		def queryExpression:String
		
		def specificParameters = Map(
				"MaxNumberOfItems" -> maxNumberOfItems.toString,
				"NextToken" -> nextToken,
				"QueryExpression" -> queryExpression
			)
	}
	
	trait QueryWithAttributes extends RequiredParameters {
		import Attributes._
		
		def action = "QueryWithAttributes"
		def attributes:Set[String]
		def maxNumberOfItems:int
		def nextToken:String
		def queryExpression:String
		
		def specificParameters = Map(
				"MaxNumberOfItems" -> maxNumberOfItems.toString,
				"NextToken" -> nextToken,
				"QueryExpression" -> queryExpression
			) ++ attributeNames(attributes)
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
	}
}