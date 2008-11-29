/**
Copyright 2008 Robin Barooah

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
**/

import scala.collection.immutable.HashMap

package org.sublime.amazon.simpleDB {
	
	class QueryParameters (private val m :Map[String,String])
	{
		/**
		 * Adding two sets of query parameters together just involves adding together the two maps.
		 */
		def + (o:QueryParameters):QueryParameters = new QueryParameters(m ++ o.m)

		/**
		 * Convert a map of query parameters into a url encoded string.
		 */
		import _root_.java.net.URLEncoder.{encode=>jencode}
		override def toString = m map (a=>a._1 + "=" + jencode(a._2)) mkString ("?", "&", "")
	}
	
	object QueryParameters {
		
		/**
		 * Encode a map of name value pairs into url query parameters.
		 */
		def apply (m :Map[String, String]):QueryParameters = new QueryParameters(m)
		def apply (mappings :(String,String)*):QueryParameters = apply(new HashMap() ++ mappings)
	}
}
