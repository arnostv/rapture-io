/**************************************************************************************************
Rapture I/O Library
Version 0.6.0

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

package rapture.io

import language.implicitConversions

import java.io._
import java.net._

import annotation.implicitNotFound

trait LowPriorityWrappers { this: Io =>
  
  /** Type class object for creating an Input[Byte] from a Java InputStream */
  implicit object InputStreamBuilder extends InputBuilder[InputStream, Byte] {
    def input(s: InputStream) = new ByteInput(s)
  }

  /** Type class object for creating an Output[Byte] from a Java Reader */
  implicit object OutputStreamBuilder extends OutputBuilder[OutputStream, Byte] {
    def output(s: OutputStream) = new ByteOutput(s)
  }

}

/** Provides wrappers around Java's standard stream classes: `InputStream`, `OutputStream`, `Reader`
  * `Writer`, generalising them into `Input`s and `Output`s parameterised by the type of data they
  * carry. */
trait Wrappers extends LowPriorityWrappers { this: Io =>

  /** Wraps a `java.io.Reader` as an `Input[Char]` */
  class CharInput(in: Reader) extends Input[Char] {

    private val bin = new BufferedReader(in)

    def ready() = bin.ready()
    
    def read() = bin.read() match {
      case -1 => None
      case x => Some(x.toChar)
    }
    
    def close() = bin.close()
    
    override def readBlock(array: Array[Char], offset: Int = 0, length: Int = -1): Int =
      bin.read(array, offset, if(length == -1) (array.length - offset) else length)
  
    override def toString() = "<character input>"
  }

  /** Wraps a `java.io.InputStream` as an `Input[Byte]` */
  class ByteInput(in: InputStream) extends Input[Byte] {
    
    private val bin = new BufferedInputStream(in)

    // FIXME: This might be really slow
    def ready() = bin.available() > 0
    
    def read() = bin.read() match {
      case -1 => None
      case x => Some(x.toByte)
    }
    
    override def readBlock(array: Array[Byte], offset: Int = 0, length: Int = -1): Int =
      bin.read(array, offset, if(length == -1) (array.length - offset) else length)

    def close() = in.close()
  
    override def toString() = "<byte input>"
  }
  
  /** Wraps a `java.io.OutputStream` into an `Output[Byte]`
    *
    * @param out The `java.io.OutputStream` to be wrapped */
  class ByteOutput(out: OutputStream) extends Output[Byte] {
    
    private val bout = new BufferedOutputStream(out)
    
    def write(b: Byte) = bout.write(b)
    
    def flush(): Unit = bout.flush()
    def close(): Unit = bout.close()
    
    override def toString() = "<byte output>"
    
    override def writeBlock(array: Array[Byte], offset: Int = 0, length: Int = -1): Int = {
      val len = if(length == -1) (array.length - offset) else length
      bout.write(array, offset, len)
      bout.flush()
      len
    }

  }

  /** Wraps a `java.io.Writer`
    *
    * @param out The `java.io.Writer` to be wrapped */
  class CharOutput(out: Writer) extends Output[Char] {
    
    private val bout = new BufferedWriter(out)
    
    def write(b: Char) = bout.write(b)
    def flush(): Unit = bout.flush()
    def close(): Unit = bout.close()
    override def toString() = "<character output>"
    
    override def writeBlock(array: Array[Char], offset: Int = 0, length: Int = -1): Int = {
      val len = if(length == -1) (array.length - offset) else length
      bout.write(array, offset, len)
      bout.flush()
      len
    }

  }

  /** Wraps a `java.io.BufferedWriter` for providing line-by-line output of `String`s
    *
    * @param out The `java.io.Writer` to be wrapped */
  class LineOutput(writer: Writer) extends Output[String] {
    def this(os: OutputStream, encoding: Encodings.Encoding) =
      this(new OutputStreamWriter(os, encoding.name))
    private val out = new BufferedWriter(writer)

    def write(s: String) = {
      out.write(s)
      out.write("\n")
    }

    def flush(): Unit = out.flush()
    def close(): Unit = out.close()
    override def toString() = "<string output>"
  }

  /** Views an `Input[Byte]` as a `java.io.InputStream` */
  implicit def inputStreamUnwrapper(is: Input[Byte]) =
    new InputStream { def read() = is.read().map(_.toInt).getOrElse(-1) }

  /** Wraps a `java.io.Reader` as an `Input[String]`, where each String item read from the stream
    * is a line of characters delimited by a newline.  This is roughly equivalent to a
    * `java.io.BufferedReader`.
    *
    * @constructor takes the Java Reader to be wrapped
    * @param reader The Java Reader instance being wrapped. */
  class LineInput(reader: Reader) extends Input[String] {
    def this(is: InputStream, encoding: Encodings.Encoding) =
      this(new InputStreamReader(is, encoding.name))
    private val in = new BufferedReader(reader)

    def ready(): Boolean = in.ready()

    /** Reads one line from the stream as a `String` */
    def read() = in.readLine.fromNull

    /** Closes the input stream and underlying `BufferedReader` */
    def close() = in.close()
  }

  /** Type class object for creating an Input[Char] from a Java Reader */
  implicit object ReaderBuilder extends InputBuilder[Reader, Char] {
    def input(s: Reader) = new CharInput(s)
  }

  /** Type class object for creating an Input[String] from a Java Reader */
  implicit object LineReaderBuilder extends InputBuilder[Reader, String] {
    def input(s: Reader) = new LineInput(s)
  }

  /** Type class object for creating an Output[Char] from a Java Writer */
  implicit object WriterBuilder extends OutputBuilder[Writer, Char] {
    def output(s: Writer) = new CharOutput(s)
  }

  implicit object AppenderBuilder extends AppenderBuilder[Writer, Char] {
    def appendOutput(s: Writer) = new CharOutput(s)
  }

  /** Type class definition for creating an Output[Char] from a Java OutputStream, taking an
    * [[Encodings.Encoding]] implicitly for converting between `Byte`s and `Char`s */
  implicit def outputStreamCharBuilder(implicit encoding: Encodings.Encoding) =
    new OutputBuilder[OutputStream, Char] {
      def output(s: OutputStream) = new CharOutput(new OutputStreamWriter(s, encoding.name))
    }

  /** Type class definition for creating an Input[Char] from a Java InputStream, taking an
    * [[Encodings.Encoding]] implicitly for converting between `Byte`s and `Char`s */
  implicit def inputStreamCharBuilder(implicit encoding: Encodings.Encoding) =
    new InputBuilder[InputStream, Char] {
      def input(s: InputStream) = new CharInput(new InputStreamReader(s, encoding.name))
    }
}
