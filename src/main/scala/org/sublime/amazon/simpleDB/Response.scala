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
	import scala.xml._
	import XMLFields._
	import SimpleDBReader._
	
	class Errors (implicit xml:NodeSeq) {
	    val error = new Error() (node("Error"))	    
	}
	
	object Error {
	    /** Extractor so we can pattern match errors **/
	    def unapply (xml:NodeSeq) :Option[(String, String, Option[Double])] = {
	        val element = node("Errors") (xml)
	        if (element.length > 0) {
	            val error = (new Errors() (element)).error
	            Some((error.code, error.message, error.boxUsage))
	        } else None
	    }
	}
	
	class Error (implicit xml:NodeSeq) {
	    val code = string("Code")
	    val message = string("Message")
	    val boxUsage = optionalDouble("BoxUsage")
	}
	
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
	
	class DomainMetadataResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new DomainMetadataResult() (node("DomainMetadataResult"))
	}
	
	class PutAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	
	class BatchPutAttributesResponse (implicit xml:NodeSeq)
	    extends SimpleDBResponse
	
	class DeleteAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
		
	class GetAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new GetAttributesResult() (node("GetAttributesResult"))
	}

	class QueryResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new QueryResult() (node("QueryResult"))
	}
	
	class QueryWithAttributesResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new QueryWithAttributesResult() (node("QueryWithAttributesResult"))
	}	
	
	
	
	abstract class ItemWithAttributesResult (implicit xml:NodeSeq) {
		class Item (implicit xml:NodeSeq) {
		    import Format._		    
			val name = string("Name")
			val attributes:Map[String, Set[String]] = readAttributes
			
			override def toString = name + "\n" + ("-" * name.length) + "\n" +
			    formatAttributes(attributes)
		}	    

		val nextToken = optionalString("NextToken")
		val items = nodes("Item") map (new Item()(_))
		
		override def toString = items mkString "\n\n"
	}
	
    class QueryResult (implicit xml:NodeSeq) {
        val itemNames = strings("ItemName")
        val nextToken = optionalString("NextToken")
            
        override def toString = itemNames mkString ", "
    }
        
	class QueryWithAttributesResult (implicit xml:NodeSeq) extends ItemWithAttributesResult
	
	class SelectResult (implicit xml:NodeSeq) extends ItemWithAttributesResult 
	
	class SelectResponse (implicit xml:NodeSeq)
		extends SimpleDBResponse
	{
		val result = new QueryWithAttributesResult() (node("SelectResult"))
	}
	
	class GetAttributesResult (implicit xml:NodeSeq) {		
	    import Format._
		val attributes:Map[String, Set[String]] = readAttributes
		
		override def toString = formatAttributes(attributes)
	}
	
	class ListDomainsResult (implicit xml:NodeSeq) {
		val domainNames = strings("DomainName")
		val nextToken = optionalString("NextToken")
		
		override def toString = domainNames mkString ("\n")
	}
	
	class DomainMetadataResult (implicit xml:NodeSeq) {
		
		// oddly this field is listed in the documentation
		// but isn't in the real responses
		// val creation = dateField("CreationDateTime")		
		
		val itemCount = int("ItemCount")
		val itemNameSizeBytes = int("ItemNamesSizeBytes")
		val attributeNameCount = int("AttributeNameCount")
		val attributeNameSizeBytes = int("AttributeNamesSizeBytes")
		val attributeValueCount = int("AttributeValueCount")
		val attributeValueSizeBytes = int("AttributeValuesSizeBytes")
		val timestamp = int("Timestamp")
		
		override def toString = List (
				//"created: " + creation,
				"items: " + itemCount,
				"item names in bytes: " + itemNameSizeBytes,
				"attibute names: " + attributeNameCount,
				"attibute names in bytes: " + attributeNameSizeBytes,
				"attribute value count: " + attributeValueCount,
				"attribute value size in bytes: " + attributeValueSizeBytes,
				"timestamp: " + timestamp
			) mkString ("\n")
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
			var found:Map[String,Set[String]] = new HashMap[String,Set[String]]()
						
			def add(name:String, value:String) {
				found = found updated (name, (found getOrElse(name, Set())) + value)
			}
			
			for (node <- nodes("Attribute")) 
				add(string("Name")(node), string("Value")(node))			    			
			
			found
		}		
	}
	
	object Format {
	    def formatAttributes(map:Map[String, Set[String]]) =
	        (map.keys map ( n => n + ": " + (map(n) mkString ", "))) mkString "\n"	    
	}
	
	/**
	 * functions for breaking down XML
	 */ 
	object XMLFields {
				
		def node (name:String) (implicit xml:NodeSeq) = (xml \ name)
		def nodes (name:String) (implicit xml:NodeSeq) = (xml \ name)
		def string (name:String) (implicit xml:NodeSeq) = node(name) text
		def optionalString (name:String) (implicit xml:NodeSeq) :Option[String] = {
		    val found = string(name)
		    if (found.length > 0) Some(found)
		    else None
		}
		def strings (name:String) (implicit xml:NodeSeq) = nodes(name) map (_.text)
		def dateField (name:String) (implicit xml:NodeSeq) = dateFormat.parse(string(name))
		def int (name:String) (implicit xml:NodeSeq) = Integer.parseInt(string(name))
		def double (name:String) (implicit xml:NodeSeq) = java.lang.Double.parseDouble(string(name))
		def optionalDouble (name:String) (implicit xml:NodeSeq) :Option[Double] = {
		    val found = string(name)
		    if (found.length > 0) Some(java.lang.Double.parseDouble(found))
		    else None
		}
		def boolean (name:String) (implicit xml:NodeSeq) = string(name) match { 
				case "True" => true
				case "False" => false
			}				
		
		import java.text.SimpleDateFormat
		def dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")		
	}	
}
