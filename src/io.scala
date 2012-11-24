/**************************************************************************************************
Rapture I/O Library
Version 0.7.1

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

import language.implicitConversions
import language.higherKinds

import java.io._
import java.net._

/** Combines different elements of the I/O framework.  This class provides implementations of
  * type class objects which should be given higher priority than the defaults.  This allows
  * methods which stream from URLs which have alternative means of being read to favour one type
  * of stream over another without explicitly specifying a type parameter.  Specifically,
  * `FileUrl`s should be read and written and  `HttpUrl`s should be read as
  * byte-streams */
abstract class Io extends Paths with Streams with Urls with Files with Net with Sockets with Extractors
    with Accumulators with Wrappers with Uris with Mail with CollectionExtras with Multipart with
    JsonExtraction with Encryption with Codecs with Digests with Encodings with Generation with Ips
    with Logging with Mime with Misc with Services with Time with Linking with Classpath {

  type **[_]

  protected def **[T](t: => T): **[T]
  
  protected def unwrapResult[T](t: => **[T]): T

  implicit def provideUnwrap[T](t: **[T]): T = unwrapResult(t)

  /** Type class object for reading `Byte`s from `FileUrl`s */
  implicit object FileStreamByteReader extends StreamReader[FileUrl, Byte] {
    def input(url: FileUrl): **[Input[Byte]] =
      **(new ByteInput(new BufferedInputStream(new FileInputStream(url.javaFile))))
  }

  /** Type class object for writing `Byte`s to `FileUrl`s */
  implicit object FileStreamByteWriter extends StreamWriter[FileUrl, Byte] {
    def output(url: FileUrl): **[Output[Byte]] =
      **(new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile))))
  }

  implicit object FileStreamByteAppender extends StreamAppender[FileUrl, Byte] {
    def appendOutput(url: FileUrl): **[Output[Byte]] =
      **(new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile, true))))
  }

  /** Type class object for reading `Byte`s from `HttpUrl`s */
  implicit object HttpStreamByteReader extends StreamReader[HttpUrl, Byte] {
    def input(url: HttpUrl): **[Input[Byte]] =
      **(new ByteInput(new BufferedInputStream(url.javaConnection.getInputStream)))
  }

  implicit def stdoutWriter[Data] = new StreamWriter[Stdout[Data], Data] {
    override def doNotClose = true
    def output(stdout: Stdout[Data]): **[Output[Data]] = **(stdout.output)
  }

  implicit def stderrWriter[Data] = new StreamWriter[Stderr[Data], Data] {
    override def doNotClose = true
    def output(stderr: Stderr[Data]): **[Output[Data]] = **(stderr.output)
  }

  implicit def stdin[Data] = new StreamReader[Stdin[Data], Data] {
    override def doNotClose = true
    def input(stdin: Stdin[Data]): **[Input[Data]] = **(stdin.input)
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

object io extends Io {
  type **[T] = T
  @inline protected def **[T](t: => T): T = t
  @inline protected def unwrapResult[T](t: => T): T = t
}

object iox extends Io {
  type **[T] = Either[Exception, T]
  
  @inline protected def **[T](t: => T): Either[Exception, T] =
    try Right(t) catch { case e: Exception => Left(e) }
  
  @inline protected def unwrapResult[T](t: => Either[Exception, T]): T =
    t.right.getOrElse(throw t.left.get)
}

