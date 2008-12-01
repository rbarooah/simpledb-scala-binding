package org.sublime.amazon.simpleDB {
	import scala.xml._
	import XMLFields._
	
	class GetAttributesResult (implicit xml:NodeSeq) {
		val attributes:Map[String, Set[String]] = readAttributes

		private def readAttributes = {
			import scala.collection.immutable.HashMap
			var found:HashMap[String,Set[String]] = new HashMap[String,Set[String]]()
			
			def add(name:String, value:String) {
				found update (name, (found getOrElse(name, Set())) + value)
			}
			
			for (node <- nodes("Attribute")) 
				add(string("Name")(node), string("Value")(node))
				
			found
		}
	}
	
	class ListDomainsResult (implicit xml:NodeSeq) {
		val domainNames = strings("DomainName")
		val nextToken = string("NextToken")
	}
	
	class ResponseMetaData (implicit xml:NodeSeq) {
		val requestId = string("RequestId")
		val boxUsage = double("BoxUsage")
	}
	
	/**
	 * Trait for breaking down XML
	 */ 
	object XMLFields {
				
		def node (name:String) (implicit xml:NodeSeq) = (xml \ name)
		def nodes (name:String) (implicit xml:NodeSeq) = (xml \ name)
		def string (name:String) (implicit xml:NodeSeq) = node(name) text
		def strings (name:String) (implicit xml:NodeSeq) = nodes(name) map (_.text)
		def dateField (name:String) (implicit xml:NodeSeq) = dateFormat.parse(string(name))
		def int (name:String) (implicit xml:NodeSeq) = Integer.parseInt(string(name))
		def double (name:String) (implicit xml:NodeSeq) = java.lang.Double.parseDouble(string(name))
		def boolean (name:String) (implicit xml:NodeSeq) = string(name) match { 
				case "True" => true
				case "False" => false
			}				
		
		import java.text.SimpleDateFormat
		def dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")		
	}	
}