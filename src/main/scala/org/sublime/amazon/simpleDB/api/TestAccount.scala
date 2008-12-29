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

package org.sublime.amazon.simpleDB.api {
    import scala.xml._
    
    /**
     * Convenient way to read test account details from a properties file on the classpath useful
     * for running tests or operating from the console.
     */
    object Test {
        import java.util.Properties
        
        private val resource = "/simpleDBTest.properties"
        private val idProperty = "awsAccessKeyId"
        private val keyProperty = "awsSecretKey"
        
        private def propertyMissing (p:String) = 
            throw new RuntimeException(resource + " does not define "+p)
        
        private def using [T] (p:String) (next: String => T) (implicit prop:Properties) :T = {
            val v = prop.getProperty(p)
            if (v == null) propertyMissing(p)
            else next(v)
        }         
        
        def loadAccount :SimpleDBAccount = {
            val is = getClass.getResourceAsStream(resource)
            if (is == null) throw new RuntimeException("resource "+resource+" not on classpath")
            else {
                implicit val p = new Properties()
                p.load(is)
                using (idProperty) { id =>
                    using (keyProperty) { key =>
                        new SimpleDBAccount(id, key)
                    }
                }
            }
        }
    }    
}