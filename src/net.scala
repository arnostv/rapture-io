/**************************************************************************************************
Rapture I/O Library
Version 0.6.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2012 Propensive Ltd.

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

  class HttpResponse(val headers : Map[String, List[String]], val code : Int, is : InputStream) {
    def input[Data](implicit ib : InputBuilder[InputStream, Data]) = ib.input(is)
  }

  trait PostType[-C] {
    def contentType : MimeTypes.MimeType
    def sender(content : C) : Input[Byte]
  }

  implicit val FormPostType = new PostType[Map[String, String]] {
    def contentType = MimeTypes.`application/x-www-form-urlencoded`
    def sender(content : Map[String, String]) = ByteArrayInput((content map { case (k, v) =>
        k.urlEncode+"="+v.urlEncode } mkString "&").getBytes("UTF-8"))
  }

  implicit val StringPostType = new PostType[String] {
    def contentType = MimeTypes.`text/plain`
    def sender(content : String) = ByteArrayInput(content.getBytes("UTF-8"))
  }

  /** Common methods for `HttpUrl`s and `HttpsUrl`s. */
  trait NetUrl[+U <: Url[U]] { netUrl : U =>
    
    private[io] def javaConnection =
      new URL(toString).openConnection().asInstanceOf[HttpURLConnection]
    
    def hostname : String
    def port : Int

    /** Sends an HTTP post to this URL.
      *
      * @param content the content to post to the URL
      * @param authenticate the username and password to provide for basic HTTP authentication,
      *        defaulting to no authentication.
      * @return the HTTP response from the remote host */
    def post[C : PostType](content : C, authenticate : Option[(String, String)] = None) :
        HttpResponse = {

      val conn = netUrl.javaConnection
      conn.setRequestMethod("POST")
      conn.setDoOutput(true)
      conn.setUseCaches(false)

      if(authenticate.isDefined) conn.setRequestProperty("Authorization",
          Base64.encode((authenticate.get._1+":"+authenticate.get._2).getBytes("UTF-8"),
          endPadding = true).mkString)

      conn.setRequestProperty("Content-Type", implicitly[PostType[C]].contentType.name)

      ensuring(OutputStreamBuilder.output(conn.getOutputStream)) { out =>
        implicitly[PostType[C]].sender(content) > out
      } (_.close())

      import scala.collection.JavaConversions._

      new HttpResponse(mapAsScalaMap(conn.getHeaderFields()).toMap.mapValues(_.toList),
          conn.getResponseCode(), conn.getInputStream())
    }
  }

  /** Represets a URL with the http scheme */
  class HttpUrl(val urlBase : NetUrlBase[HttpUrl], elements : Seq[String]) extends Url[HttpUrl](elements)
      with NetUrl[HttpUrl] with PathUrl[HttpUrl] { thisHttpUrl =>
    
    def makePath(xs : Seq[String]) = new HttpUrl(urlBase, elements)
    def hostname = urlBase.hostname
    def port = urlBase.port
  }

  /** Represents a URL with the https scheme */
  class HttpsUrl(val urlBase : NetUrlBase[HttpsUrl], elements : Seq[String]) extends Url[HttpsUrl](elements)
      with NetUrl[HttpsUrl] with PathUrl[HttpsUrl] { thisHttpsUrl =>
    
    def makePath(xs : Seq[String]) = new HttpsUrl(urlBase, elements)
    def hostname = urlBase.hostname
    def port = urlBase.port
  }

  trait NetUrlBase[+T <: Url[T] with NetUrl[T]] extends UrlBase[T] {
    def hostname : String
    def port : Int
  }

  class HttpUrlBase(val hostname : String, val port : Int) extends
      NetUrlBase[HttpUrl] { thisUrlBase =>
    
    override def toString() = scheme.schemeName+"://"+hostname+(if(port == 80) "" else ":"+port)
    
    def makePath(elements : Seq[String]) : HttpUrl = new HttpUrl(thisUrlBase, elements)

    def scheme = Http
    
    override def /(element : String) = makePath(Array(element))
    
    def /(path : Path) = makePath(path.elements)
    
    override def equals(that : Any) : Boolean =
      that.isInstanceOf[HttpUrlBase] && hostname == that.asInstanceOf[HttpUrlBase].hostname
  }

  class HttpsUrlBase(val hostname : String, val port : Int) extends NetUrlBase[HttpsUrl]
      { thisUrlBase =>
    
    override def toString() = scheme.schemeName+"://"+hostname+(if(port == 443) "" else ":"+port)
    
    def makePath(elements : Seq[String]) : HttpsUrl = new HttpsUrl(thisUrlBase, elements)
    
    def scheme = Https
    
    override def /(element : String) = makePath(Array(element))
    
    def /(path : Path) = makePath(path.elements)
    
    override def equals(that : Any) : Boolean =
      that.isInstanceOf[HttpsUrlBase] && hostname == that.asInstanceOf[HttpsUrlBase].hostname
  }

  /** Factory for creating new HTTP URLs */
  object Http extends Scheme[HttpUrl] {
    def schemeName = "http"
    
    /** Creates a new URL with the http scheme with the specified domain name and port
      *
      * @param hostname A `String` of the domain name for the URL
      * @param port The port to connect to this URL on, defaulting to port 80 */
    def /(hostname : String, port : Int = Services.Tcp.http.portNo) =
      new HttpUrlBase(hostname, port)
  
    private val UrlRegex = """(https?):\/\/([\.\-a-z0-9]+)(:[1-9][0-9]*)?(\/.*)""".r

    // FIXME: This should be generalised to all schemes
    /** Parses a URL string into an HttpUrl or HttpsUrl */
    def parse(s : String) : Option[Either[HttpUrl, HttpsUrl]] = s match {
      case UrlRegex(scheme, server, port, path) =>
        val rp = new Path(0, path.split("/"))
        val p = if(port == null) 80 else port.substring(1).toInt
        Some(scheme match {
          case "http" => Left(Http./(server, p) / rp)
          case "https" => Right(Https./(server, p) / rp)
        })
      case _ => None
    }
  }

  /** Factory for creating new HTTPS URLs */
  object Https extends Scheme[HttpsUrl] {
    def schemeName = "https"

    /** Creates a new URL with the https scheme with the specified domain name and port
      *
      * @param hostname A `String` of the domain name for the URL
      * @param port The port to connect to this URL on, defaulting to port 443 */
    def /(hostname : String, port : Int = Services.Tcp.https.portNo) =
      new HttpsUrlBase(hostname, port)
  }
}

