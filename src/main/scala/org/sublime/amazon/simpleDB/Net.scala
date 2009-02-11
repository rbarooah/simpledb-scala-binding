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
    import Request._
	import scala.xml._

	import org.apache.commons.httpclient.HttpClient
	import org.apache.commons.pool._
	import org.apache.commons.pool.impl._

	object Service {
		val url = "https://sdb.amazonaws.com"
	}

	private class ConnectionProvider extends BasePoolableObjectFactory {
		override def makeObject () = {
		    new HttpClient();
		}
	}

	private class ConnectionPool {
		private val pool = {
			val p = new GenericObjectPool(new ConnectionProvider)
			p.setMaxActive(-1)
			p.setMaxIdle(100)
			p.setTimeBetweenEvictionRunsMillis(5 * 60 * 100) // 5 minute eviction runs
			p
		}

		def exec[T] (f: (HttpClient) => T) : T = {
			val client = pool.borrowObject.asInstanceOf[HttpClient]
			try {
			    return f(client);
			} finally {
			    pool.returnObject(client);
			}
		}
	}

	class Connection (val awsAccessKeyId:String, awsSecretKey:String) {
		import Service._
		import Exceptions.toException
		
		import org.apache.commons.httpclient.methods.{GetMethod, PostMethod}
		
		private val signer = new Signer(awsSecretKey)
		private val pool = new ConnectionPool()
		
		var trace = false
				
		def makeRequest (request:SimpleDBRequest) :Elem = {
		    if (trace) diagnose(request.parameters)
			val method = 
				new PostMethod(url + 
					QueryParameters(signer.sign(request.parameters)))

			pool.exec((c) => {
				c.executeMethod(method)
				val xml = XML.load(method.getResponseBodyAsStream())
				method.releaseConnection
				if (trace) diagnose(xml)
				xml match {
					case Error(code, message, boxUsage) =>
						throw toException(code, message, boxUsage)
					case _ => xml
				}
			})
		}
		
		def printer = new PrettyPrinter(80, 2)
		
		def diagnose (xml:Node) {
		    Console.println(printer.format(xml))
		}
		
		def diagnose (parameters:Map[String, String]) {
		    Console.println(
		        (parameters.keys map (k => k + ": "+parameters(k))) mkString "\n"
		    )
		}						
	}	
}