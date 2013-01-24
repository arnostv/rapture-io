/**************************************************************************************************
Rapture I/O Library
Version 0.7.2

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2013 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture

import language.implicitConversions
import language.higherKinds

import scala.reflect.ClassTag

import java.io._
import java.net._

/** Combines different elements of the I/O framework.  This class provides implementations of
  * type class objects which should be given higher priority than the defaults.  This allows
  * methods which stream from URLs which have alternative means of being read to favour one type
  * of stream over another without explicitly specifying a type parameter.  Specifically,
  * `FileUrl`s should be read and written and  `HttpUrl`s should be read as
  * byte-streams */
abstract class BaseIo extends Paths with Streams with Urls with Files with Net with Sockets with
    Extractors with Accumulators with Wrappers with Uris with Mail with CollectionExtras with
    Multipart with JsonExtraction with Encryption with Codecs with Digests with Encodings with
    Generation with Ips with Logging with Mime with Misc with Services with Time with Linking with
    Classpath with Processes with CommandLine with Tabulation with Exceptions {

  type ![_ <: Exception, _]

  protected def except[E <: Exception, T](t: => T)(implicit mf: ClassTag[E]): ![E, T]
  
  protected def unexcept[E <: Exception, T](t: => ![E, T]): T

  implicit def autoUnexcept[E <: Exception, T](t: ![E, T]): T = unexcept[E, T](t)

  /** Type class object for reading `Byte`s from `FileUrl`s */
  implicit object FileStreamByteReader extends StreamReader[FileUrl, Byte] {
    def input(url: FileUrl): ![Exception, Input[Byte]] =
      except[Exception, Input[Byte]](new ByteInput(new BufferedInputStream(
          new FileInputStream(url.javaFile))))
  }

  /** Type class object for writing `Byte`s to `FileUrl`s */
  implicit object FileStreamByteWriter extends StreamWriter[FileUrl, Byte] {
    def output(url: FileUrl): ![Exception, Output[Byte]] =
      except(new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile))))
  }

  implicit object FileStreamByteAppender extends StreamAppender[FileUrl, Byte] {
    def appendOutput(url: FileUrl): ![Exception, Output[Byte]] =
      except(new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile, true))))
  }

  /** Type class object for reading `Byte`s from `HttpUrl`s */
  implicit object HttpStreamByteReader extends StreamReader[HttpUrl, Byte] {
    def input(url: HttpUrl): ![Exception, Input[Byte]] =
      except(new ByteInput(new BufferedInputStream(url.javaConnection.getInputStream)))
  }

  implicit def stdoutWriter[Data] = new StreamWriter[Stdout[Data], Data] {
    override def doNotClose = true
    def output(stdout: Stdout[Data]): ![Exception, Output[Data]] =
      except[Exception, Output[Data]](stdout.output)
  }

  implicit def stderrWriter[Data] = new StreamWriter[Stderr[Data], Data] {
    override def doNotClose = true
    def output(stderr: Stderr[Data]): ![Exception, Output[Data]] =
      except[Exception, Output[Data]](stderr.output)
  }

  implicit def stdin[Data] = new StreamReader[Stdin[Data], Data] {
    override def doNotClose = true
    def input(stdin: Stdin[Data]): ![Exception, Input[Data]] =
      except[Exception, Input[Data]](stdin.input)
  }

  implicit class urlCodec(s: String) {
    @inline def urlEncode(implicit encoding: Encoding = Encodings.`UTF-8`) =
      URLEncoder.encode(s, encoding.name)
    @inline def urlDecode(implicit encoding: Encoding = Encodings.`UTF-8`) =
      URLDecoder.decode(s, encoding.name)
  }

  // FIXME Move to misc
  @inline implicit class NullableExtras[T](t: T) {
    def fromNull = if(t == null) None else Some(t)
  }

  def randomGuid() = java.util.UUID.randomUUID().toString

  def DevNull[T] = new Output[T] {
    def close() = ()
    def flush() = ()
    def write(t: T) = ()
  }

}

object io extends BaseIo {
  type ![E <: Exception, T] = T
  @inline protected def except[E <: Exception, T](t: => T)(implicit mf: ClassTag[E]): T = t
  @inline protected def unexcept[E <: Exception, T](t: => T): T = t
}

object iox extends BaseIo {
  type ![E <: Exception, T] = Either[E, T]
  
  @inline protected def except[E <: Exception, T](t: => T)(implicit mf: ClassTag[E]): Either[E, T] =
    try Right(t) catch {
      case e: E => Left(e)
      case e: Throwable => throw e
    }
  
  @inline protected def unexcept[E <: Exception, T](t: => Either[E, T]): T =
    t.right.getOrElse(throw t.left.get)
}

