package org.sublime.amazon.simpleDB {
	import scala.xml._
	import XMLFields._
	import SimpleDBReader._
	
	class SimpleDBResponse (implicit xml:NodeSeq) {
		val metadata = readMetadata
	}
	
	class CreateDomainResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse 
	
	class DeleteDomainResponse (implicit xml:NodeSeq) 
		extends SimpleDBResponse
		
	class ListDomainsResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new ListDomainsResult() (node("ListDomainsResult"))
	}
	
	class PutAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	
	class DeleteAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
		
	class GetAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new GetAttributesResult()
	}

	class QueryResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new QueryResult()
	}
	
	class QueryWithAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new QueryWithAttributesResult()
	}	
	
	class QueryWithAttributesResult (implicit xml:NodeSeq) {
		class Item (implicit xml:NodeSeq) {
			val name = string("Name")
			val attributes:Map[String, Set[String]] = readAttributes
		}
		
		val items = nodes("Item") map (new Item()(_))
	}
	
	class QueryResult (implicit xml:NodeSeq) {
		val itemNames = strings("ItemName")
	}
	
	class GetAttributesResult (implicit xml:NodeSeq) {		
		val attributes:Map[String, Set[String]] = readAttributes
	}
	
	class ListDomainsResult (implicit xml:NodeSeq) {
		val domainNames = strings("DomainName")
		val nextToken = string("NextToken")
		
		override def toString = domainNames mkString ("\n")
	}
	
	class ResponseMetadata (implicit xml:NodeSeq) {
		val requestId = string("RequestId")
		val boxUsage = double("BoxUsage")
		
		override def toString = "Box Usage: "+boxUsage+"s"+" request id: "+requestId
	}
	
	/**
	 * Functions for decomposing simpleDB specific types.
	 */
	object SimpleDBReader {
		def readMetadata (implicit xml:NodeSeq) =
		 	new ResponseMetadata()(node("ResponseMetadata"))
		
		def readAttributes (implicit xml:NodeSeq) = {
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
	
	/**
	 * functions for breaking down XML
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