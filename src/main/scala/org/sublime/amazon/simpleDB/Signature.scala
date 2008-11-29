package org.sublime.amazon.simpleDB {
	
	class Signer(key:String)
	{
		import scala.util.Sorting._
		import javax.crypto.spec.SecretKeySpec
		
		val awsSecretKey = new SecretKeySpec(key.getBytes, "hmacsha1")
				
		/**
		 * Return the signature as a tuple.
		 */
		def signature (m:Map[String, String]) :(String,String) = {
		
			/**
			 * Include all the keys except for the signature itself.
			 */
			def keysToInclude = m.keys filter (!_.equals("Signature"))
		
			/**
			 * Sort the query names case insensitively.
			 */
			def sortedKeys = stableSort(keysToInclude.toList,
				(a:String, b:String) => {
						a.toUpperCase.compareTo(b.toUpperCase) < 0
					}
				)
			
			/**
			 * Combine the list.
			 */
			def combinedParameters = sortedKeys map {
				(param:String) => {
					param + m(param)	
				}
			} mkString	
		
			/**
			 * Get the hash of it.
			 */
			import javax.crypto.Mac
			def digest = {
				val mac = Mac.getInstance("hmacsha1")
				mac.init(awsSecretKey)
				mac.update(combinedParameters.getBytes)
				mac.doFinal
			}
		
			/**
			 * And base64 encode it.
			 */
			import org.apache.commons.codec.binary.Base64
			val base64Encoder = new Base64
			def encode = {
				base64Encoder.encode(digest)
			}
			
			// Return the signature as a pair
//			"Signature" -> encode.toString
			"signature" -> "other string"
		}
	}	
}