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
     * A simple concrete implementation of the simpleDB API.  Instantiate one of these with
     * your AWS Credentials and then use the methods defined in the SimpleAPI trait.
     */
    class SimpleDBAccount (val awsAccessKeyId:String, awsSecretKey:String) extends SimpleAPI
    {
        val connection = new Connection(awsAccessKeyId, awsSecretKey)        
        def makeSimpleDBRequest (req:SimpleDBRequest) :Elem = connection.makeRequest(req)
    }
}