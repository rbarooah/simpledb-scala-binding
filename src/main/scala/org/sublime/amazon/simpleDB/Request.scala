package org.sublime.amazon.simpleDB {
	
	trait RequiredParameters {
		def action:String
		def awsAccessKeyId:String
		def domainName:String
		def signature:String
		def signatureVersion:String
		def timeStamp:String
		def version:String
	}
	
	trait CreateDomainRequest extends RequiredParameters {
		def action = "CreateDomain"
	}
	
	trait DeleteDomainRequest extends RequiredParameters {
		def action = "DeleteDomain"
	}
	
	trait ListDomains extends RequiredParameters {
		def action = "ListDomains"
		def maxNumberOfDomains:Option[String]
		def nextToken:Option[String]
	}
	
	trait PutAttributes extends RequiredParameters {
		def action = "PutAttributes"
		def itemName:String
		def attributes:Map[String, (String, Boolean)]
	}
	
	trait DeleteAttributes extends RequiredParameters {
		def action = "DeleteAttributes"
		def attributes:Map[String, Set[String]]
	}
	
	trait GetAttributes extends RequiredParameters {
		def action = "GetAttributes"
		def itemName:String
		def attributeName:Set[String]
	}
	
	trait Query extends RequiredParameters {
		def action = "Query"
		def maxNumberOfItems:int
		def nextToken:String
		def queryExpression:String
		
	}
	
	trait QueryWithAttributes extends RequiredParameters {
		def action = "QueryWithAttributes"
		def attributes:Set[String]
		def maxNumberOfItems:int
		def nextToken:String
		def queryExpression:String
	}
}