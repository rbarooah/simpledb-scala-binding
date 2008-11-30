package org.sublime.amazon.simpleDB {
	
	/**
	 * Class to sign simpleDB REST http requests.
	 */ 
	class Signer(key:String)
	{
		import scala.util.Sorting._
		import javax.crypto.spec.SecretKeySpec
		
		val awsSecretKey = new SecretKeySpec(key.getBytes, "hmacsha1")
		
		/**
		 * Given a map of request parameters, return a signed map containing
		 * the same parameters.  This is idempotent and will return a correct value map even if what is
		 * passed in is already signed.
		 */
		def sign (m:Map[String, String]) = {
			// the parameters with the actual signature and version added
			val versioned = m update ("SignatureVersion", "1")
			versioned + signature(versioned)
		}
				
		/**
		 * Return the signature of a map of request parameters as a tuple.
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
			"Signature" -> new String(encode)
		}
	}	
}