/**************************************************************************************************
Rapture I/O Library
Version 0.6.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2011 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture.io

import java.io._
import java.net._

/** Provides functionality for handling internet URLs, namely HTTP and HTTPS schemes. */
trait Net { this : Io =>

  /** Common methods for `HttpUrl`s and `HttpsUrl`s. */
  trait NetUrl[+U <: Url[U]] { netUrl : U  =>
    
    private[io] def javaConnection =
      new URL(toString).openConnection().asInstanceOf[HttpURLConnection]
    
    /** Sends an HTTP post to this URL.
      *
      * @param contentType the MIME type of the request content
      * @param authenticate the username and password to provide for basic HTTP authentication,
      *        defaulting to no authentication.
      * @param sender a block of code to write data to the `Output` for the HTTP post.
      * @return a pair consisting of the response code of this request and an [[rapture.io.Input]]
      *         resulting from this HTTP post. */
    def post[P, R](contentType : MimeTypes.MimeType, authenticate : Option[(String, String)] =
        None)(sender : Output[P] => Unit)(implicit encoding : Encodings.Encoding, ib :
        InputBuilder[InputStream, R], ob : OutputBuilder[OutputStream, P]) : (Int, Input[R]) = {

      val conn = netUrl.javaConnection
      conn.setRequestMethod("POST")
      conn.setDoOutput(true)
      conn.setUseCaches(false)
      
      if(authenticate.isDefined) conn.setRequestProperty("Authorization",
          Base64.encode((authenticate.get._1+":"+authenticate.get._2).getBytes("UTF-8"),
          endPadding = true).mkString)
      
      conn.setRequestProperty("Content-Type", contentType.name)
      
      ensuring(ob.output(conn.getOutputStream))(sender)(_.close())
      
      (conn.getResponseCode(), ib.input(conn.getInputStream))
    }

    /** Imitates an HTTP form post to this URL.
      *
      * @usecase def formPost[Data](params : Seq[(String, String)]) : (Int, Input[Data])
      * @param params a sequence of key/value pairs for the form parameters to send
      * @param authenticate the username and password to provide for basic HTTP authentication,
      *        defaulting to no authentication.
      * @tparam Data the type of data which will be streamed back, typically `Byte` or `Char`
      & @return a pair consisting of the response code of this request and an [[rapture.io.Input]]
      *         resulting from this HTTP post. */
    def formPost[Data](params : Seq[(String, String)],
        authenticate : Option[(String, String)] = None)(implicit encoding : Encodings.Encoding,
        ib : InputBuilder[InputStream, Data]) : (Int, Input[Data]) = {

      post(MimeTypes.`application/x-www-form-urlencoded`, authenticate) { out : Output[Char] =>
        out.writeBlock((params map { case (k, v) => k.urlEncode+"="+v.urlEncode } mkString
            "&").toCharArray)
        out.flush()
      }
    }
  }


  /** Represets a URL with the http scheme */
  class HttpUrl(val urlBase : UrlBase[HttpUrl], val elements : Seq[String]) extends Url[HttpUrl]
      with NetUrl[HttpUrl] with PathUrl[HttpUrl] { thisHttpUrl =>
    
    def makePath(xs : Seq[String]) = new HttpUrl(urlBase, elements)
  }

  /** Represents a URL with the https scheme */
  class HttpsUrl(val urlBase : UrlBase[HttpsUrl], val elements : Seq[String]) extends Url[HttpsUrl]
      with NetUrl[HttpsUrl] with PathUrl[HttpsUrl] { thisHttpsUrl =>
    
    def makePath(xs : Seq[String]) = new HttpsUrl(urlBase, elements)
  }

  class HttpUrlBase(val domainName : String, val port : Int) extends
      UrlBase[HttpUrl] { thisUrlBase =>
    
    override def toString() = scheme.schemeName+"://"+domainName+(if(port == 80) "" else ":"+port)
    
    def makePath(elements : Seq[String]) : HttpUrl = new HttpUrl(thisUrlBase, elements)

    def scheme = Http
    
    def /(element : String) = makePath(Array(element))
    
    def /(path : RelativePath) = makePath(path.elements)
    
    override def equals(that : Any) : Boolean =
      that.isInstanceOf[HttpUrlBase] && domainName == that.asInstanceOf[HttpUrlBase].domainName
  }

  class HttpsUrlBase(val domainName : String, val port : Int) extends UrlBase[HttpsUrl]
      { thisUrlBase =>
    
    override def toString() = scheme.schemeName+"://"+domainName+(if(port == 443) "" else ":"+port)
    
    def makePath(elements : Seq[String]) : HttpsUrl = new HttpsUrl(thisUrlBase, elements)
    
    def scheme = Https
    
    def /(element : String) = makePath(Array(element))
    
    def /(path : RelativePath) = makePath(path.elements)
    
    override def equals(that : Any) : Boolean =
      that.isInstanceOf[HttpsUrlBase] && domainName == that.asInstanceOf[HttpsUrlBase].domainName
  }

  /** Factory for creating new HTTP URLs */
  object Http extends Scheme[HttpUrl]("http") {
    /** Creates a new URL with the http scheme with the specified domain name and port
      *
      * @param domainName A `String` of the domain name for the URL
      * @param port The port to connect to this URL on, defaulting to port 80 */
    def /(domainName : String, port : Int = Services.Tcp.http.portNo) =
      new HttpUrlBase(domainName, port)
  
    private val UrlRegex = """(https?):\/\/([\.\-a-z0-9]+)(:[1-9][0-9]*)?(\/.*)""".r

    // FIXME: This should be generalised to all schemes
    /** Parses a URL string into an HttpUrl or HttpsUrl */
    def parse(s : String) : Option[Either[HttpUrl, HttpsUrl]] = s match {
      case UrlRegex(scheme, server, port, path) =>
        val rp = new RelativePath(0, path.split("/"))
        Some(scheme match {
          case "http" => Left(Http./(server, port.substring(1).toInt) / rp)
          case "https" => Right(Https./(server, port.substring(1).toInt) / rp)
        })
      case _ => None
    }
  }

  /** Factory for creating new HTTPS URLs */
  object Https extends Scheme[HttpsUrl]("https") {
    /** Creates a new URL with the https scheme with the specified domain name and port
      *
      * @param domainName A `String` of the domain name for the URL
      * @param port The port to connect to this URL on, defaulting to port 443 */
    def /(domainName : String, port : Int = Services.Tcp.https.portNo) =
      new HttpsUrlBase(domainName, port)
  }
}

