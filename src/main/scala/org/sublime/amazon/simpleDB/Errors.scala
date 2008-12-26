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
    
    object Exceptions {
    
        class SimpleDBException(val code:String, val message:String, val boxUsage:Double)
            extends Exception(code+": "+message+" ("+boxUsage+"s)")
 
        class ServerException(code:String, message:String, boxUsage:Double) 
            extends SimpleDBException(code, message, boxUsage)
 
        class InternalError(code:String, message:String, boxUsage:Double) 
            extends ServerException(code, message, boxUsage)	 
        
        class ServiceUnavailable(code:String, message:String, boxUsage:Double) 
            extends ServerException(code, message, boxUsage)
 
        class ClientException(code:String, message:String, boxUsage:Double) 
            extends SimpleDBException(code, message, boxUsage)    
                    
        class AccessFailure(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class AuthFailure(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class AuthMissingFailure(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class FeatureDeprecated(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class InvalidAction(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class InvalidBatchRequest(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
            
        class InvalidHTTPAuthHeader(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class InvalidHttpRequest(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class InvalidLiteral(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class InvalidNextToken(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
    
        class InvalidNumberPredicates(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
            
        class InvalidNumberValueTests(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)    
        
        class InvalidParameterCombination(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)    
        
        class InvalidParameterValue(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)    
        
        class InvalidQueryExpression(code:String, message:String, boxUsage:Double)
            extends ServerException(code, message, boxUsage)
            
        class InvalidResponseGroups(code:String, message:String, boxUsage:Double)
            extends ServerException(code, message, boxUsage)
            
        class InvalidService(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)        
    
        class InvalidSOAPRequest(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class InvalidURI(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class InvalidWSAddressingProperty(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class MalformedSOAPSignature(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class MissingAction(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class MissingParameter(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class MissingWSAddressingProperty(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NoSuchDomain(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NoSuchVersion(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NotYetImplemented(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NumberDomainsExceeded(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NumberDomainAttributesExceeded(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NumberDomainBytesExceeded(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NumberItemAttributesExceeded(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class NumberSubmittedAttributesExceeded(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class RequestExpired(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class RequestTimeout(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class TooManyRequestedAttributes(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class UnsupportedHttpVerb(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class UnsupportedNextToken(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)
        
        class URITooLong(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)        
    }
}