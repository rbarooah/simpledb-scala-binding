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
    
        class DuplicateItemName(code:String, message:String, boxUsage:Double)
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

        class NumberSubmittedItemsExceeded(code:String, message:String, boxUsage:Double)
            extends ClientException(code, message, boxUsage)

        class QueryTimeout(code:String, message:String, boxUsage:Double)
            extends ServerException(code, message, boxUsage)
        
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
 
        def toException(code:String, message:String, usage:Option[Double]) = 
            makeException(code, message, usage getOrElse 0.0)
            
        def makeException(code:String, message:String, boxUsage:Double) = code match {  
                      
            case "AccessFailure" => 	
            	new AccessFailure(code, message, boxUsage)

            case "AuthFailure" => 
            	new AuthFailure(code, message, boxUsage)

            case "AuthMissingFailure" => 
            	new AuthMissingFailure(code, message, boxUsage)

            case "DuplicateItemName" =>
                new DuplicateItemName(code, message, boxUsage)

            case "FeatureDeprecated" => 
            	new FeatureDeprecated(code, message, boxUsage)

            case "InternalError" =>  
            	new InternalError(code, message, boxUsage)

            case "InvalidAction" =>  
            	new InvalidAction(code, message, boxUsage)

            case "InvalidBatchRequest" => 
            	new InvalidBatchRequest(code, message, boxUsage)

            case "InvalidHTTPAuthHeader" => 
            	new InvalidHTTPAuthHeader(code, message, boxUsage)

            case "InvalidHttpRequest" => 
            	new InvalidHttpRequest(code, message, boxUsage)

            case "InvalidLiteral" => 
            	new InvalidLiteral(code, message, boxUsage)

            case "InvalidNextToken" => 
            	new InvalidNextToken(code, message, boxUsage)

            case "InvalidNumberPredicates" => 
            	new InvalidNumberPredicates(code, message, boxUsage)

            case "InvalidNumberValueTests" => 
            	new InvalidNumberValueTests(code, message, boxUsage)

            case "InvalidParameterCombination" => 
            	new InvalidParameterCombination(code, message, boxUsage)

            case "InvalidParameterValue" => 
            	new InvalidParameterValue(code, message, boxUsage)

            case "InvalidQueryExpression" => 
            	new InvalidQueryExpression(code, message, boxUsage)

            case "InvalidResponseGroups" => 
            	new InvalidResponseGroups(code, message, boxUsage)

            case "InvalidService" => 
            	new InvalidService(code, message, boxUsage)

            case "InvalidSOAPRequest" => 
            	new InvalidSOAPRequest(code, message, boxUsage)

            case "InvalidURI" => 
            	new InvalidURI(code, message, boxUsage)

            case "InvalidWSAddressingProperty" => 
            	new InvalidWSAddressingProperty(code, message, boxUsage)

            case "MalformedSOAPSignature" => 
            	new MalformedSOAPSignature(code, message, boxUsage)

            case "MissingAction" => 
            	new MissingAction(code, message, boxUsage)

            case "MissingParameter" => 
            	new MissingParameter(code, message, boxUsage)

            case "MissingWSAddressingProperty" => 
            	new MissingWSAddressingProperty(code, message, boxUsage)

            case "NoSuchDomain" => 
            	new NoSuchDomain(code, message, boxUsage)

            case "NoSuchVersion" => 
            	new NoSuchVersion(code, message, boxUsage)

            case "NotYetImplemented" => 
            	new NotYetImplemented(code, message, boxUsage)

            case "NumberDomainsExceeded" => 
            	new NumberDomainsExceeded(code, message, boxUsage)

            case "NumberDomainAttributesExceeded" => 
            	new NumberDomainAttributesExceeded(code, message, boxUsage)

            case "NumberDomainBytesExceeded" => 
            	new NumberDomainBytesExceeded(code, message, boxUsage)

            case "NumberItemAttributesExceeded" => 
            	new NumberItemAttributesExceeded(code, message, boxUsage)

            case "NumberSubmittedAttributesExceeded" => 
            	new NumberSubmittedAttributesExceeded(code, message, boxUsage)
            	
            case "NumberSubmittedItemsExceeded" =>
                new NumberSubmittedItemsExceeded(code, message, boxUsage)
            	
            case "QueryTimeout" =>
              new QueryTimeout(code, message, boxUsage)

            case "RequestExpired" => 
            	new RequestExpired(code, message, boxUsage)

            case "RequestTimeout" => 
            	new RequestTimeout(code, message, boxUsage)

            case "ServiceUnavailable" => 
            	new ServiceUnavailable(code, message, boxUsage)

            case "TooManyRequestedAttributes" => 
            	new TooManyRequestedAttributes(code, message, boxUsage)

            case "UnsupportedHttpVerb" => 
            	new UnsupportedHttpVerb(code, message, boxUsage)

            case "UnsupportedNextToken" => 
            	new UnsupportedNextToken(code, message, boxUsage)

            case "URITooLong" => 
            	new URITooLong(code, message, boxUsage)
        }
    }
}