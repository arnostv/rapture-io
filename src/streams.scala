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

import annotation.implicitNotFound

trait Streams { this : Io =>

  /** Safely closes a stream after processing */
  def ensuring[Result, Stream](create : Stream)(body : Stream => Result)(close : Stream => Unit) = {
    val stream = create
    val result = try { body(stream) } catch {
      case e : Throwable => try { close(stream) } catch { case e2 : Exception => () }
      throw e
    }
    close(stream)

    result
  }

  case class Stdout[Data](implicit outputBuilder : OutputBuilder[OutputStream, Data]) {
    def output = outputBuilder.output(System.out)
  }

  case class Stderr[Data](implicit outputBuilder : OutputBuilder[OutputStream, Data]) {
    def output = outputBuilder.output(System.err)
  }
  
  case class Stdin[Data](implicit inputBuilder : InputBuilder[InputStream, Data]) {
    def input = inputBuilder.input(System.in)
  }

  /** Makes a `String` viewable as an `rapture.io.Input[Char]` */
  case class StringInput(string : String) extends CharInput(new StringReader(string))

  /** Makes an `Array[Byte]` viewable as an `Input[Byte]` */
  case class ByteArrayInput(array : Array[Byte]) extends ByteInput(new ByteArrayInputStream(array))

  /** Makes a string viewable as an `Input` */
  implicit def stringToInput(string : String) = StringInput(string)

  /** Type trait for building a new `Input` from particular kind of input stream
    *
    * @tparam InputType The type of input that is to be interpreted as an `Input`,
    *         such as `java.io.InputStream` or `java.io.Reader`
    * @tparam Data The type of data that the `Input` carries */
  trait InputBuilder[InputType, Data] { def input(s : InputType) : Input[Data] }
  
  /** Type trait for building a new `Output[Data]` from particular kind of output stream
    *
    * @tparam OutputType The type of output that is to be interpreted as an `Output`,
    *         such as [[java.io.OutputStream]] or [[java.io.Writer]]
    * @tparam Data The type of data that the [[Output]] carries */
  trait OutputBuilder[OutputType, Data] { def output(s : OutputType) : Output[Data] }

  /** Provides methods for URLs which can be read as streams */
  implicit def makeReadable[UrlType](url : UrlType) : Readable[UrlType] =
    new Readable(url)
  
  class Readable[UrlType](url : UrlType) {
    
    /** Gets the input for the resource specified in this URL */
    def input[Data](implicit sr : StreamReader[UrlType, Data]) = sr.input(url)
    
    /** Pumps the input for the specified resource to the destination URL provided */
    def pumpTo[Data, DestUrlType](dest : DestUrlType)(implicit sr :
        StreamReader[UrlType, Data], sw : StreamWriter[DestUrlType, Data], mf : Manifest[Data]) = {

      handleInput[Data, Int] { in =>
        writable(dest).handleOutput[Data, Int] { out => in.pumpTo(out) }
      }
    }
 
    /** Pumps the input for the specified resource to the destination output provided
      *
      * @tparam Data The type that the data should be pumped as
      * @param output The destination for data to be pumped to */
    def pumpTo[Data](output : Output[Data])(implicit sr : StreamReader[UrlType, Data],
        mf : Manifest[Data]) = handleInput[Data, Int] { in => in.pumpTo(output) }

    /** Carefully handles writing to the input stream, ensuring that it is closed following
      * data being written to the stream. Handling an input stream which is already being handled
      * will have no effect.
      *
      * @tparam Data The type of data the stream should carry
      * @tparam Result The type of body's result
      * @param body The code to be executed upon the this Input before it is closed */
    def handleInput[Data, Result](body : Input[Data] => Result)(implicit sr :
        StreamReader[UrlType, Data]) : Result = {
     
      ensuring(input[Data](sr))(body) { in => if(!sr.doNotClose) in.close() }
    }

    def md5Sum()(implicit sr : StreamReader[UrlType, Byte]) =
      Md5.digestHex(slurp[Byte, Array[Byte]]())

    def sha256Sum()(implicit sr : StreamReader[UrlType, Byte]) =
      Sha256.digestHex(slurp[Byte, Array[Byte]]())

    /** Reads in the entirety of the stream and accumulates it into an appropriate object
      * depending on the availability of implicit Accumulator type class objects in scope.
      *
      * @usecase def slurp[Char, String]() : String
      * @usecase def slurp[Byte, Array[Byte]]() : Array[Byte]
      * @tparam Data The units of data being slurped
      * @tparam Acc The type of the object data will be accumulated into
      * @return The accumulated data */
    def slurp[Data, Acc]()(implicit sr : StreamReader[UrlType, Data], AccumulatorBuilder :
        AccumulatorBuilder[Data, Acc], mf : Manifest[Data]) = {

      val c = AccumulatorBuilder.make()
      input[Data].pumpTo(c)

      c.buffer
    }
  }

  /** Provides methods for URLs which can be written to as streams, most importantly for getting an
    * `Output` */
  implicit def writable[UrlType](url : UrlType) : Writable[UrlType] = new Writable[UrlType](url)
  
  class Writable[UrlType](url : UrlType) {
    
    /** Gets the output stream directly
      *
      * @tparam Data The type of data to be carried by the `Output` */
    def output[Data](implicit sw : StreamWriter[UrlType, Data]) = sw.output(url)
    
    /** Carefully handles writing to the output stream, ensuring that it is closed following
      * data being written.
      *
      * @param body The code to be executed upon this `Output` before being closed.
      * @return The result from executing the body */
    def handleOutput[Data, Result](body : Output[Data] => Result)(implicit sw :
        StreamWriter[UrlType, Data]) : Result = {
      
      ensuring(output[Data])(body) { out =>
        out.flush()
        if(!sw.doNotClose) out.close()
      }
    }
  }

  /** Type trait for defining how a URL of type U should 
    *
    * @tparam Url Url for which this corresponds
    * @tparam Data Units of data to be streamed, typically `Byte` or `Char` */
  @implicitNotFound(msg = "Cannot find implicit StreamWriter for ${UrlType} Urls. ${UrlType} URLs "+
      "can only be written to if a StreamWriter implicit exists within scope.")
  trait StreamWriter[-UrlType, Data] {
    def doNotClose = false
    def output(url : UrlType, append : Boolean = false) : Output[Data]
  }


  /*  Extract the encoding from an HTTP stream */
  private def extractEncoding(huc : HttpURLConnection) : String = {
    
    huc.getContentEncoding match {
      case null =>
        huc.getContentType match {
          case null =>
            Encodings.`ISO-8859-1`.name
          case ct =>
            ct.split(";") map (_.trim) find (_ startsWith "charset=") map (_ substring 8) getOrElse
                Encodings.`ISO-8859-1`.name
        }
      case ce => ce
    }
  }

  /** Type class object for getting an Input[Char] from an HttpUrl */
  implicit object HttpStreamCharReader extends StreamReader[HttpUrl, Char] {
    def input(url : HttpUrl) : Input[Char] =
      new CharInput(new BufferedReader(new InputStreamReader(url.javaConnection.getInputStream,
          extractEncoding(url.javaConnection))))
  }

  /** Type class object for getting an Input[Char] from an HttpsUrl */
  implicit object HttpsStreamCharReader extends StreamReader[HttpsUrl, Char] {
    def input(url : HttpsUrl) : Input[Char] =
      new CharInput(new BufferedReader(new InputStreamReader(url.javaConnection.getInputStream,
          extractEncoding(url.javaConnection))))
  }

  /** An Input provides an incoming stream of data */
  trait Input[@specialized(Byte, Char) Data] {

    private var beingHandled = false
    
    /** Returns whether the stream can be read without blocking */
    def ready() : Boolean

    /** Reads a single item of data from the input stream.  Note that each call to this method
      * will result in a new instance of `Some` to be constructed, so for reading larger block, use
      * the `readBlock` method which may be implemented more efficiently. */
    def read() : Option[Data]
   
    /** Default implementation for reading a block of data from the input stream into the specified
      * array.
      *
      * @param array The array into which content should be read
      * @param offset The offset to the position within the array that data should start to be
      *        written.  Defaults to the start of the array.
      * @param length The length of data to read from the stream into the array.
      * @return The number of items of data transferred */
    def readBlock(array : Array[Data], offset : Int = 0, length : Int = -1) : Int = {

      val end = if(length < 0) (array.length - offset) else (offset + length)

      read() match {
        case None => -1
        case Some(c) =>
          array(offset) = c
          var i = offset + 1
          var continue = true
          while(i < end && continue) {
            read() match {
              case None =>
                continue = false
              case Some(c) =>
                array(i) = c
                i += 1
            }
          }

          i - offset
      }
    }
   
    /** Reads the whole stream into an accumulator */
    def slurp[Acc]()(implicit accumulatorBuilder : AccumulatorBuilder[Data, Acc],
        mf : Manifest[Data]) : Acc = {
      val acc = accumulatorBuilder.make()
      pumpTo(acc)
      acc.buffer
    }

    /** Closes the input stream so that no further data will be provided. */
    def close() : Unit
    
    /** Pumps the data from this `Input[Data]` to the given destination.
      *
      * @param dest The destination URL for data to be pumped to */
    def pumpTo[UrlType <: Url[UrlType]](dest : UrlType)(implicit to : StreamWriter[UrlType, Data],
        mf : Manifest[Data]) : Int = dest.handleOutput[Data, Int] { out => pumpTo(out) }
   
    /** Pumps data from this `Input` to the specified `Output` until the end of the stream is
      * reached.
      *
      * @param out The output stream to receive data pumped from this `Input` */
    def pumpTo(out : Output[Data])(implicit mf : Manifest[Data]) : Int = {
      val buf = new Array[Data](65536)
      var len = readBlock(buf)
      var count = 0
      while(len >= 0) {
        out.writeBlock(buf, length = len)
        count += len
        len = readBlock(buf)
      }
      count
    }
  }

  /** Defines a generic output stream */
  trait Output[Data] {
    private var beingHandled = false
   
    /** Writes one item of data to this stream
      *
      * @param data The data to be written */
    def write(data : Data) : Unit
  
    /** Writes a block of data from an array to the stream
      *
      * @param array the Array containing the data to be written to the stream
      * @param offset The offset to the position within the array that data should start to be
      *        read from when writing to the stream.  Defaults to the start of the array.
      * @param length The length of data to write from the array into the stream. Defaults to the
      *        remainder of the array.
      * @return The number of data items written. */
    def writeBlock(array : Array[Data], offset : Int = 0, length : Int = -1) : Int = {
      
      val end = if(length < 0) (array.length - offset) else (offset + length)
      array.slice(offset, end).foreach(write)

      end - offset
    }
   
    /** Flushes the stream */
    def flush() : Unit

    /** Closes the stream */
    def close() : Unit
  }
 
  /** Generic type class for reading a particular kind of data from 
    */
  @implicitNotFound(msg = "Cannot find implicit StreamReader for ${UrlType} Urls. ${UrlType} URLs "+
      "can only be read from if a StreamReader implicit exists within scope.")
  trait StreamReader[-UrlType, Data] {
  
    def doNotClose = false

    /** Creates the `Input` for streaming data of the specified type from the given URL
      *
      * @param url The URL to get the input stream from
      * @return an `Input[Data]` for the specified URL */
    def input(url : UrlType) : Input[Data]
    
    /** Pumps data from the specified URL to the given destination URL */
    def pump[DestUrlType <: Url[DestUrlType]](url : UrlType, dest : DestUrlType)(implicit sw :
      StreamWriter[DestUrlType, Data], mf : Manifest[Data]) : Int = input(url).pumpTo(dest)
  }

  /** Type class object for reading `Char`s from a `String` */
  implicit object StringCharReader extends StreamReader[String, Char] {
    def input(s : String) : Input[Char] = StringInput(s)
  }

}
