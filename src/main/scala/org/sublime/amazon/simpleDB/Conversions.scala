package org.sublime.amazon.simpleDB {
    import java.util.Date
        
    object Conversions {
        
        object PassThrough extends Conversion[String] {
            def apply (value:String) = value
            def unapply (value:String) = Some(value)
        }

        object PositiveInt extends Conversion[int] {
            import java.text.DecimalFormat
    
            val format = new DecimalFormat("0" * Integer.MAX_VALUE.toString.length)
    
            def apply (number:int) = 
                if (number >= 0) format.format(number)
                else throw new IllegalArgumentException("field can only be positive but was "+number)
    
            def unapply (string:String) :Option[int] = {
                val number = format.parse(string)
                if (number == null) None
                else Some(number.intValue)
            }        
        }
    
        object ISO8610Date extends Conversion[Date] {
        	import java.text.SimpleDateFormat

        	val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")		
    
            def apply (date:Date) :String = format.format(date)
    
            def unapply (string:String) :Option[Date] = {
                val date = format.parse(string)
                if (date == null) None
                else Some(date)
            }
        }
        
        object SHA1Base64 extends Conversion[String] {
            import java.security.MessageDigest
            
            import org.apache.commons.codec.binary.Base64
			private def base64Encode :(Array[Byte]) => String = 
			    (bytes) => (new Base64).encode(bytes).toString
						
			private def digest :(String) => Array[Byte] =
			    (in) => MessageDigest.getInstance("SHA-1").digest(in.getBytes)
			       
            def apply (in:String) :String = base64Encode(digest(in))
            
            def unapply (in:String) :Option[String] = None
        }
    }

    trait Conversion [T] {
        def apply (t:T) :String
        def unapply (s:String) :Option[T]
    }
}