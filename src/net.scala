/**************************************************************************************************
Rapture I/O Library
Version 0.7.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2012 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture

import java.io._
import java.net._

/** Provides functionality for handling internet URLs, namely HTTP and HTTPS schemes. */
trait Net { this: Io =>

  object HttpMethods {
    
    private val methods = new scala.collection.mutable.HashMap[String, Method]
    
    sealed class Method(val string: String) {
      
      def unapply(r: String) = r == string
      override def toString = string
      
      methods += string -> this
    }

    trait FormMethod { this: Method => }

    def method(s: String) = methods(s)

    val Get = new Method("GET") with FormMethod
    val Put = new Method("PUT")
    val Post = new Method("POST") with FormMethod
    val Delete = new Method("DELETE")
    val Trace = new Method("TRACE")
    val Options = new Method("OPTIONS")
    val Head = new Method("HEAD")
    val Connect = new Method("CONNECT")
    val Patch = new Method("PATCH")

  }

  class HttpResponse(val headers: Map[String, List[String]], val status: Int, is: InputStream) {
    def input[Data](implicit ib: InputBuilder[InputStream, Data]) = ib.input(is)
  }

  trait PostType[-C] {
    def contentType: MimeTypes.MimeType
    def sender(content: C): Input[Byte]
  }

  implicit val FormPostType = new PostType[Map[String, String]] {
    def contentType = MimeTypes.`application/x-www-form-urlencoded`
    def sender(content: Map[String, String]) = ByteArrayInput((content map { case (k, v) =>
      URLEncoder.encode(k, "UTF-8")+"="+URLEncoder.encode(v, "UTF-8")
    } mkString "&").getBytes("UTF-8"))
  }

  implicit val StringPostType = new PostType[String] {
    def contentType = MimeTypes.`text/plain`
    def sender(content: String) = ByteArrayInput(content.getBytes("UTF-8"))
  }

  implicit val JsonPostType = new PostType[Json] {
    //def contentType = MimeTypes.`application/json`
    def contentType = MimeTypes.`application/x-www-form-urlencoded`
    def sender(content: Json) =
      ByteArrayInput(content.toString.getBytes("UTF-8"))
  }

  /** Common methods for `HttpUrl`s and `HttpsUrl`s. */
  trait NetUrl[+U <: Url[U]] extends Uri { netUrl: U =>
    
    import javax.net.ssl._
    import javax.security.cert._
    
    private[rapture] def javaConnection: HttpURLConnection =
      new URL(toString).openConnection().asInstanceOf[HttpURLConnection]
    
    private val trustAllCertificates = {
      Array[TrustManager](new X509TrustManager {
        override def getAcceptedIssuers(): Array[java.security.cert.X509Certificate] = null
        def checkClientTrusted(certs: Array[java.security.cert.X509Certificate], authType: String): Unit = ()
        def checkServerTrusted(certs: Array[java.security.cert.X509Certificate], authType: String): Unit = ()
      })
    }
    
    private val sslContext = SSLContext.getInstance("SSL")
    sslContext.init(null, trustAllCertificates, new java.security.SecureRandom())

    private val allHostsValid = new HostnameVerifier {
      def verify(hostname: String, session: SSLSession) = true
    }

    def hostname: String
    def port: Int
    def ssl: Boolean
    def canonicalPort: Int

    def schemeSpecificPart = "//"+hostname+(if(port == canonicalPort) "" else ":"+port)+pathString

    /** Sends an HTTP post to this URL.
      *
      * @param content the content to post to the URL
      * @param authenticate the username and password to provide for basic HTTP authentication,
      *        defaulting to no authentication.
      * @return the HTTP response from the remote host */
    def post[C: PostType](content: C, authenticate: Option[(String, String)] = None,
        ignoreInvalidCertificates: Boolean = false, httpHeaders: Map[String, String] = Map()): HttpResponse = {

      val conn: URLConnection = new URL(toString).openConnection()
      conn match {
        case c: HttpsURLConnection =>
          if(ignoreInvalidCertificates) {
            c.setSSLSocketFactory(sslContext.getSocketFactory)
            c.setHostnameVerifier(allHostsValid)
          }
          c.setRequestMethod("POST")
          c.setDoOutput(true)
          c.setUseCaches(false)
        case c: HttpURLConnection =>
          c.setRequestMethod("POST")
          c.setDoOutput(true)
          c.setUseCaches(false)
      }

      if(authenticate.isDefined) conn.setRequestProperty("Authorization",
          Base64.encode((authenticate.get._1+":"+authenticate.get._2).getBytes("UTF-8"),
          endPadding = true).mkString)

      conn.setRequestProperty("Content-Type", implicitly[PostType[C]].contentType.name)
      for((k, v) <- httpHeaders) conn.setRequestProperty(k, v)

      ensuring(OutputStreamBuilder.output(conn.getOutputStream)) { out =>
        implicitly[PostType[C]].sender(content) > out
      } (_.close())

      import scala.collection.JavaConversions._

      val statusCode = conn match {
        case c: HttpsURLConnection => c.getResponseCode()
        case c: HttpURLConnection => c.getResponseCode()
      }
      
      val is = try conn.getInputStream() catch {
        case e: IOException => new ByteArrayInputStream("".getBytes())
      }
      new HttpResponse(mapAsScalaMap(conn.getHeaderFields()).toMap.mapValues(_.toList),
          statusCode, is)
    }
  }

  implicit object HttpQueryParameters extends QueryType[HttpUrl, Map[Symbol, String]] {
    def extras(existing: AfterPath, q: Map[Symbol, String]): AfterPath =
      existing + ('?' -> ((q.map({ case (k, v) =>
        k.name.urlEncode+"="+v.urlEncode
      }).mkString("&")) -> 1.0))
  }

  implicit object PageIdentifier extends QueryType[HttpUrl, Symbol] {
    def extras(existing: AfterPath, q: Symbol): AfterPath =
      existing + ('#' -> (q.name -> 2.0))
  }

  /** Represets a URL with the http scheme */
  class HttpUrl(val pathRoot: NetPathRoot[HttpUrl], elements: Seq[String], afterPath: AfterPath) extends
      Url[HttpUrl](elements, afterPath) with NetUrl[HttpUrl] with PathUrl[HttpUrl] { thisHttpUrl =>
    
    def makePath(ascent: Int, xs: Seq[String], afterPath: AfterPath) =
      new HttpUrl(pathRoot, elements, afterPath)
    
    def hostname = pathRoot.hostname
    def port = pathRoot.port
    def ssl = false
    def canonicalPort = 80
  }

  /** Represents a URL with the https scheme */
  class HttpsUrl(val pathRoot: NetPathRoot[HttpsUrl], elements: Seq[String], afterPath: AfterPath) extends
      Url[HttpsUrl](elements, afterPath) with NetUrl[HttpsUrl] with PathUrl[HttpsUrl] {
      thisHttpsUrl =>
    
    def makePath(ascent: Int, xs: Seq[String], afterPath: AfterPath) =
      new HttpsUrl(pathRoot, elements, afterPath)
    
    def hostname = pathRoot.hostname
    def port = pathRoot.port
    def ssl = true
    def canonicalPort = 443
  }

  trait NetPathRoot[+T <: Url[T] with NetUrl[T]] extends PathRoot[T] {
    def hostname: String
    def port: Int
  }

  class HttpPathRoot(val hostname: String, val port: Int) extends
      NetPathRoot[HttpUrl] { thisPathRoot =>
    
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): HttpUrl =
      new HttpUrl(thisPathRoot, elements, Map())

    def scheme = Http
    
    override def /(element: String) = makePath(0, Array(element), Map())
    
    def /[P <: Path[P]](path: P) = makePath(0, path.elements, Map())
    
    override def equals(that: Any): Boolean =
      that.isInstanceOf[HttpPathRoot] && hostname == that.asInstanceOf[HttpPathRoot].hostname
  }

  class HttpsPathRoot(val hostname: String, val port: Int) extends NetPathRoot[HttpsUrl]
      { thisPathRoot =>
    
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): HttpsUrl =
      new HttpsUrl(thisPathRoot, elements, afterPath)
    
    def scheme = Https
    
    override def /(element: String) = makePath(0, Array(element), Map())
    
    def /[P <: Path[P]](path: P) = makePath(0, path.elements, Map())
    
    override def equals(that: Any): Boolean =
      that.isInstanceOf[HttpsPathRoot] && hostname == that.asInstanceOf[HttpsPathRoot].hostname
  }

  /** Factory for creating new HTTP URLs */
  object Http extends Scheme[HttpUrl] {
    def schemeName = "http"
    
    /** Creates a new URL with the http scheme with the specified domain name and port
      *
      * @param hostname A `String` of the domain name for the URL
      * @param port The port to connect to this URL on, defaulting to port 80 */
    def /(hostname: String, port: Int = Services.Tcp.http.portNo) =
      new HttpPathRoot(hostname, port)
  
    private val UrlRegex = """(https?):\/\/([\.\-a-z0-9]+)(:[1-9][0-9]*)?\/?(.*)""".r

    /** Parses a URL string into an HttpUrl or HttpsUrl */
    def parse(s: String): Option[NetUrl[U forSome { type U <: Url[U] }]] = s match {
      case UrlRegex(scheme, server, port, path) =>
        val rp = new SimplePath(path.split("/"), Map())
        val p = if(port == null) 80 else port.substring(1).toInt
        Some(scheme match {
          case "http" => Http./(server, p) / rp
          case "https" => Https./(server, p) / rp
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
    def /(hostname: String, port: Int = Services.Tcp.https.portNo) =
      new HttpsPathRoot(hostname, port)
  }
}

