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
	
	/**
	 * This trait provides a simple API for accessing Amazon SimpleDB.  It is designed to be
	 * mixed in to objects or classes that provide the necessary networking capabilities. 
	 * 
	 * @see SimpleDBAccount for a simple and complete implementation.
	 */
	trait SimpleAPI extends Concrete
	{	
	    import scala.collection.MapProxy
	    		
	    /**
	     * A map implementation which holds a snapshot of the properties and values of an item.
	     * An item object which can be used for updates or to make further queries can be accessed
	     * via the 'item' field.
	     */
	    class ItemSnapshot(val item:Item, val self:Map[String,Set[String]]) 
	        extends MapProxy[String,Set[String]]	    
	    
	    /**
	     * A map implementation which holds a snapshot of the properties and values of an item
	     * The 'name' field contains the name of the item.
	     */
	    class ItemNameSnapshot(val name:String, val self:Map[String,Set[String]])
	        extends MapProxy[String,Set[String]]
	    
	    /**
	     * Perform a select operation and return a stream of results.  The results are simple
	     * maps of attributes names to sets of values.  A single request is made initially, and
	     * additional requests are made as needed when the stream is read.
	     */
        def select (expression:String) 
		    :Stream[ItemNameSnapshot] = {
		        
		    def convert (i:ItemWithAttributesResult#Item) = new ItemNameSnapshot(i.name, 
		        i.attributes)
		    
		    def generate(res:SelectResponse) :Stream[ItemNameSnapshot] =
		        streamOfObjects(res.result.items.toList, convert)
		        
		    def responses(req:SelectRequest, res:SelectResponse) 
		        :Stream[SelectResponse] =
		        Stream.cons(res, SelectRequest.next(req, res) match {
		            case None => Stream.empty
		            case Some(request) => responses(request, request.response)			            
		        })
		        
		    val start = SelectRequest.start(expression)
		    streamOfStreams(responses(start, start.response), generate)        		
	    }
	    
	    /**
	     * A class which serves as a proxy to an Item within simpleDB.  This class holds none of the
	     * attributes of the item itself.  Calls to methods which read or write attributes to and
	     * from the item will result in network requests to SimpleDB.
	     */
		class Item(val domain:Domain, val name:String) {

            /**
             * Return a string assocating this item with it's domain in the form "domain.item"
             */
		    def path = domain + "." + name
		    
		    override def toString = name
		    
			/**
			 * Read all of the attributes from this item.
			 */
			def attributes = new ItemSnapshot(this, 
			    (new GetAttributesRequest(domain.name, name, Set())).response.result.attributes
			)
			
			/**
			 * Read a selection of attributes from this item
			 */
			def attributes (attributes:Set[String]) = new ItemSnapshot(this,
				(new GetAttributesRequest(domain.name, name, Set())).response.result.attributes
			)
				
			/**
			 * Read a single attribute from this item.
			 */
			def attribute (attributeName:String) =
				(new GetAttributesRequest(domain.name, name, Set(attributeName)))
					.response.result.attributes(attributeName)
			
			private def putAttribute (pair :(String, String), replace:Boolean) = {
				(new PutAttributesRequest(domain.name, name, 
						Map(pair._1 -> (Set(pair._2) -> replace))
					)
				).response.metadata				
			}
					
			private def putAttribute (attributeName:String, values:Set[String], replace:Boolean) = {
			    (new PutAttributesRequest(domain.name, name, 
			            Map(attributeName -> (values, replace))
			        )
			    ).response.metadata
			}
			
			/**
			 * Update the contents of this item with a map of attributes names and a set of values
			 * and boolean for each one.  If the boolean is true, the existing values will be
			 * replace with those in the set, otherwise the set of values will be added to the
			 * existing ones.
			 *
			 * This is the analog of the 'PutAttributes' request.
			 */
			def update (values:Map[String, (Set[String], Boolean)]) = {
			    (new PutAttributesRequest(domain.name, name, values)).response.metadata
			}
					
			/**
			 * Add a single value to an attribute of this item.
			 */
			def += (pair:(String, String)) = putAttribute(pair, false)

            /**
             * Add multiple values to this attribute by specifying a series of mappings.
             */
            def += (pairs:(String, String)*) = update(combinePairs(false, pairs:_*))

            private def combinePairs (replace:Boolean, pairs:(String, String)*)
                :Map[String, (Set[String], Boolean)] = {
                def combine (map:Map[String,(Set[String], Boolean)], pair:(String, String))
                    :Map[String,(Set[String], Boolean)] = 
                        if (map.contains(pair._1)) 
                            map ++ Map(pair._1 -> (map(pair._1)._1 + pair._2 -> replace))
                        else map ++ Map(pair._1 -> (Set[String](pair._2) -> replace))

                (Map[String,(Set[String], Boolean)]() /: pairs) (combine(_, _))
            }

			/**
			 * Add multiple values to an attribute of this item.
			 */
			def += (name:String, values:Set[String]) = putAttribute(name, values, false)

			/**
			 * Replace the value of an attribute in this item with a single value.  Any previously
			 * existing values for this item will be deleted.
			 */
			def set (pair:(String,String)) = putAttribute(pair, true)
				
		    /**
		     * Replace the value of an attribute in this item with a set of values.  Any previously
		     * existing values for this item will be deleted.
		     */
		    def set (name:String, values:Set[String]) = putAttribute(name, values, true)
		
		    /**
		     * Replace the values of multiple attributes in this item with a series of mappings.
		     */
		    def set (pairs:(String,String)*) = update(combinePairs(true, pairs:_*))
		
			/** 
			 * Delete all of the attributes in this item.
			 */
			def clear = {
				(new DeleteAttributesRequest(domain.name, name, Map()).response.metadata)
			}
			
			/**
			 * Delete a single attribute value pair in this item.
			 */
			def -= (pair :(String, String)) = {
				(new DeleteAttributesRequest(domain.name, name, Map(pair._1 -> Set(pair._2))))
					.response.metadata
			}		
			
			/**
			 * Delete a single attribute in this item.
			 */
			def -= (attributeName:String) = {
				(new DeleteAttributesRequest(domain.name, attributeName, Map(name -> Set())))
					.response.metadata
			}
		}
		
		/**
	     * A class which serves as a proxy to a Doman within simpleDB.  This class holds no data
	     * other than a reference to the domain name.  Calls to methods which access items from
	     * within the domain will always result in network requests to SimpleDB.
	     */
		class Domain(val name:String) {
		    
		    /**
		     * Return a current snapshot of the metadata associated with this domain.
		     *
		     * This is the analog of the 'DomainMetadata' request.
		     */
			def metadata = (new DomainMetadataRequest(name)).response.result
					
			/**
			 * Delete this domain from the SimpleDB account.
			 *
			 * This is the analog of the 'DeleteDomain' request.
			 */
			def delete = (new DeleteDomainRequest(name)).response.metadata
			
			/**
			 * Create a domain within SimpleDB corresponding to this object if one doesn't exist
			 * already.
			 *
			 * This is the analog of the 'CreateDomain'
			 */
			def create = (new CreateDomainRequest(name)).response.metadata
			
			/**
			 * Return a reference to an item with a given name within this domain. 
			 */
			def item (name:String) = new Item(this, name)
			
			/**
			 * Return a reference to an item given an ItemNameSnapshot (as returned from select)
			 */
			def item (snapshot:ItemNameSnapshot) = new Item(this, snapshot.name)
			
			/**
			 * Perform a request and return a stream of the results.  One simpleDB request will be
			 * performed initially, and subsequent queries will be performed as the stream is read
			 * if they are needed.
			 *
			 * This is the analog of the 'Query' request.
			 */
			def query (expression:String) :Stream[Item] = query(Some(expression))
			
			/**
			 * Return a stream containing all of the items within the domain.  One simpleDB request
			 * will be perfomed initially, and subsequent queries will be performed as the stream
			 * is read if they are needed.  This query does not obtain any of the attributes but
			 * returns Item objects that you can use to retrieve the attributes you desire.
			 *
			 * This the exact analog of using the 'Query' request without specifying a query
			 * expression.
			 */
			def items :Stream[Item] = query(None)
			
			private def query(expression:Option[String]) :Stream[Item] = {
			    def generate (res:QueryResponse) :Stream[Item] =
			        streamOfObjects[Item, String](res.result.itemNames.toList, item)
			        
			    def responses (req:QueryRequest, res:QueryResponse) :Stream[QueryResponse] =
			        Stream.cons(res, QueryRequest.next(req, res) match {
			            case None => Stream.empty
			            case Some(request) => responses(request, request.response)
			        })
			        
			    val start = QueryRequest.start(name, expression)
			    streamOfStreams(responses(start, start.response), generate)
			}
			
			/**
			 * Return a stream containing all of the items within the domain with all of their
			 * attributes.  As with most of the queries that return multiple results, a lazy stream
			 * is returned and additional requests are made to SimpleDB only when needed.
			 *
			 * This is the analog of using the 'QueryWithAttributes' request without specifying a
			 * query expression.
			 */
			def itemsWithAttributes :Stream[ItemSnapshot] = withAttributes (Set[String]())
			
			/**
			 * Return a stream containing the items matching a given query with all of their
			 * attributes.  As with most of the queries that return multiple results, a lazy stream
			 * is returned and additional requests are made to SimpleDB only when needed.
			 *
			 * This is the analog of using the 'QueryWithAttributes' request with a query expression
			 * but no list of attributes.
			 */
			def withAttributes (expression:String) :Stream[ItemSnapshot] 
			    = withAttributes(Some(expression), Set[String]())
			
			/**
			 * Return a stream containing all of the items within a domain with a selected set of 
			 * their attributes.  As with most of the queries that return multiple results, a lazy
			 * stream is returned and additional requests are made to SimpleDB only when needed.
			 *
			 * This is the analog of using the 'QueryWithAttributes' request without a query 
			 * expression but with a list of attributes.
			 */
			def withAttributes (attributes:Set[String]) :Stream[ItemSnapshot] 
			    = withAttributes(None, attributes)
			
			/**
			 * Return a stream containing the items matching a given query with a selected set of 
			 * their attributes.  As with most of the queries that return multiple results, a lazy
			 * stream is returned and additional requests are made to SimpleDB only when needed.
			 *
			 * This is the analog of using the 'QueryWithAttributes' request with a query 
			 * expression and a list of attributes.
			 */
			def withAttributes (expression:String, attributes:Set[String]) :Stream[ItemSnapshot] =			 
			    withAttributes (Some(expression), attributes)
			
			
			/**
			 * Return a stream containing the items matching an optional query with a selected set 
			 * of their attributes.  As with most of the queries that return multiple results, a 
			 * lazy stream is returned and additional requests are made to SimpleDB only when 
			 * needed.  If 'None' is supplied instead of the query string, all items are returned.
			 *
			 * This is the analog of using the 'QueryWithAttributes' request.
			 */			
			def withAttributes (expression:Option[String], attributes:Set[String]) 
			    :Stream[ItemSnapshot] = {
			    def convert (i:QueryWithAttributesResult#Item) = 
			        new ItemSnapshot(item(i.name), i.attributes)
			        
			    def generate(res:QueryWithAttributesResponse) :Stream[ItemSnapshot] =
			        streamOfObjects(res.result.items.toList, convert)
			        
			    def responses(req:QueryWithAttributesRequest, res:QueryWithAttributesResponse) 
			        :Stream[QueryWithAttributesResponse] =
			        Stream.cons(res, QueryWithAttributesRequest.next(req, res) match {
			            case None => Stream.empty
			            case Some(request) => responses(request, request.response)			            
			        })
			        
			    val start = QueryWithAttributesRequest.start(name, expression, attributes)
			    streamOfStreams(responses(start, start.response), generate)        		
		    }
		    			
			override def toString = name			
		}
		
		/**
		 * Return a proxy object representing the named simpleDB domain.  No request is made
		 * and the domain may or may not exist on the server. A domain may be created on the server
		 * using the 'create' method, or deleted using the 'delete' method.
		 */
		def domain (name:String) = new Domain(name)
					
		// given a stream of generators of generators, which generate type T, and a
		// function to convert a generator into a stream of T, produce a single stream
		// that yields all of the Ts from each generator.
		private def streamOfStreams [T, G] (sources:Stream[G], generate:(G => Stream[T])) :Stream[T] =
		{		  		    
		    def next (sources:Stream[G], generated:Stream[T]) :Stream[T] = 
	            if (generated.isEmpty) {
	                if (sources.isEmpty) Stream.empty
	                else next (sources.tail, generate(sources.head))
	            } else Stream.cons(generated.head, next(sources, generated.tail))
		    
		    next(sources, Stream.empty)
		}		
		
		// given a list of type K, and a function to convert from K to T, produce
		// a stream that performs the conversion as each element is accessed
		private def streamOfObjects [T, K] (list:List[K], convert: (K => T)) :Stream[T] =
		{
		    def makeStream (remaining:List[K]) :Stream[T] = {
		        remaining match {
	                case List() => Stream.empty
	                case head :: tail => Stream.cons(convert(head), makeStream(tail))
	            }
            }
	        makeStream(list)	        
		}
			
	    /**
	     * Return a stream of all of the domains within the simpleDB account.  As usual this stream
	     * is fetched lazily, and additional requests to simpleDB will be made only when needed.
	     * 
	     * The stream consists of proxy objects that can be used to make further requests.
	     */
		def domains :Stream[Domain] = {
		    def convert (name:String) = new Domain(name)
		    
		    def generate (res:ListDomainsResponse) :Stream[Domain] = 
		        streamOfObjects(res.result.domainNames.toList, convert)
		        
		    def responses (response:ListDomainsResponse) :Stream[ListDomainsResponse] =
		        Stream.cons(response, ListDomainsRequest.next(response) match {
		            case None => Stream.empty
		            case Some(request) => responses(request.response)
		        })
		        
		    streamOfStreams(responses(ListDomainsRequest.start.response), generate)
		}
	}
}