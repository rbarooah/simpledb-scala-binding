package org.sublime.amazon.simpleDB {
	import scala.xml.PrettyPrinter
	
	trait SimpleAPI extends Concrete
	{	
	    import scala.collection.MapProxy
	    		
	    class ItemSnapshot(val item:Item, val self:Map[String,Set[String]]) 
	        extends MapProxy[String,Set[String]]	    
	    
        def select (expression:String) 
		    :Stream[Map[String, Set[String]]] = {
		    def convert (i:ItemWithAttributesResult#Item) = i.attributes
		    def generate(res:SelectResponse) :Stream[Map[String,Set[String]]] =
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
	    
		class Item(val domain:Domain, val name:String) {
		    
		    override def toString = domain + "." + name
		    
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
			
			def putAttribute (pair :(String, String), replace:Boolean) = {
				(new PutAttributesRequest(domain.name, name, 
						Map(pair._1 -> (pair._2 -> replace))
					)
				).response.metadata				
			}
					
			def putAttribute (name:String, values:Set[String], replace:Boolean) = {
			    (new PutAttributesRequest(domain.name, name, 
			            Map() ++ (values map (value => (name -> (value -> replace))))
			        )
			    ).response.metadata
			}
			
			def update (values:Map[String, (String, Boolean)]) = {
			    (new PutAttributesRequest(domain.name, name, values)).response.metadata
			}
					
			/**
			 * Add a single attribute to this item.
			 */
			def += (pair:(String, String)) = putAttribute(pair, false)
			
			def += (name:String, values:Set[String]) = putAttribute(name, values, false)

			/**
			 * Replace a single attribute in this item.
			 */
			def set (pair:(String,String)) = putAttribute(pair, true)
		
		    def set (name:String, values:Set[String]) = putAttribute(name, values, true)
		
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
			def -= (name:String) = {
				(new DeleteAttributesRequest(domain.name, name, Map(name -> Set())))
					.response.metadata
			}
		}
		
		class Domain(val name:String) {
			def metadata = (new DomainMetadataRequest(name)).response.result
					
			def delete = (new DeleteDomainRequest(name)).response.metadata
			def create = (new CreateDomainRequest(name)).response.metadata
			def item (name:String) = new Item(this, name)
			
			def query (expression:String) :Stream[Item] = query(Some(expression))
			
			def items :Stream[Item] = query(None)
			
			private def query(expression:Option[String]) :Stream[Item] = {
			    def generate (res:QueryResponse) :Stream[Item] =
			        streamOfObjects(res.result.itemNames.toList, item)
			    def responses (req:QueryRequest, res:QueryResponse) :Stream[QueryResponse] =
			        Stream.cons(res, QueryRequest.next(req, res) match {
			            case None => Stream.empty
			            case Some(request) => responses(request, request.response)
			        })
			    val start = QueryRequest.start(name, expression)
			    streamOfStreams(responses(start, start.response), generate)
			}
			
			/**
			 * Get all items and all of their attributes
			 */
			def itemsWithAttributes :Stream[ItemSnapshot] = withAttributes (Set[String]())
			
			/**
			 * Get all items and their attributes 
			 */
			def withAttributes (expression:String) :Stream[ItemSnapshot] 
			    = withAttributes(Some(expression), Set[String]())
			
			def withAttributes (attributes:Set[String]) :Stream[ItemSnapshot] 
			    = withAttributes(None, attributes)
			
			def withAttributes (expression:String, attributes:Set[String]) :Stream[ItemSnapshot] =			 
			    withAttributes (Some(expression), attributes)
			
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