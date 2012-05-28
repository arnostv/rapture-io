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

/** Provides classes for creating and listening on sockets.  This code is largely incomplete. */
trait Sockets { this : Io =>

  /** Listens for incoming connections on the specified port
    *
    * @usecase def listen(port : Int) : Input[Byte]
    * @param port the port to listen to */
  def listen[K](port : Int)(implicit ib : InputBuilder[InputStream, K]) : Input[K] = {
    val sock = new java.net.ServerSocket(port)
    ib.input(sock.accept().getInputStream)
  }

  /*def listen(port : Int, local : Boolean = true, timeout : Int = 2000) = {
    val socket = new ServerSocket()
    socket.setSoTimeout(timeout)
    if(local) socket.bind(new InetSocketAddress("127.0.0.1", port))
    else socket.bind(new InetSocketAddress(port))
  }*/

  class SocketUrlBase(val hostname : String, val port : Int) extends UrlBase[SocketUrl]
      { thisUrlBase =>
    
    def scheme = Socket
    lazy val javaSocket = new java.net.Socket(hostname, port)
    override def toString() = scheme.schemeName+"://"+hostname+":"+port
    
    def makePath(elems : Seq[String]) : SocketUrl = new SocketUrl {
      val urlBase = thisUrlBase
	    val elements : Seq[String] = elems.toArray[String]
    }
  }

  object Socket extends Scheme[SocketUrl]("socket") {
    def apply(hostname : String, port : Int) = new SocketUrlBase(hostname, port)
  }


  abstract class SocketUrl extends Url[SocketUrl] {
    def javaSocket = urlBase.asInstanceOf[SocketUrlBase].javaSocket
    def makePath(elems : Seq[String]) = urlBase.makePath(elems)
  }

  /** Type class object for getting an `Output[Byte]` from a socket URL. */
  implicit object SocketStreamByteWriter extends StreamWriter[SocketUrl, Byte] {
    def output(url : SocketUrl, append : Boolean = false) : Output[Byte] =
      new ByteOutput(new BufferedOutputStream(url.javaSocket.getOutputStream))
  }
}
