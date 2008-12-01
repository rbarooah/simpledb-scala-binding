package org.sublime.amazon.simpleDB {
	import scala.xml._
	import XMLFields._
	
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