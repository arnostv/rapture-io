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

import java.io._
import java.net._

/** Combines different elements of the I/O framework.  This class provides implementations of
  * type class objects which should be given higher priority than the defaults.  This allows
  * methods which stream from URLs which have alternative means of being read to favour one type
  * of stream over another without explicitly specifying a type parameter.  Specifically,
  * `FileUrl`s should be read and written and  `HttpUrl`s and `HttpsUrl`s should be read as
  * byte-streams */
class Io extends Paths with Streams with Urls with Files with Net with Sockets with Extractors
    with Accumulators with Wrappers {

  /** Type class object for reading `Byte`s from `FileUrl`s */
  implicit object FileStreamByteReader extends StreamReader[FileUrl, Byte] {
    def input(url : FileUrl) : Input[Byte] =
      new ByteInput(new BufferedInputStream(new FileInputStream(url.javaFile)))
  }

  /** Type class object for writing `Byte`s to `FileUrl`s */
  implicit object FileStreamByteWriter extends StreamWriter[FileUrl, Byte] {
    def output(url : FileUrl) : Output[Byte] =
      new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile)))
  }

  implicit object FileStreamByteAppender extends StreamAppender[FileUrl, Byte] {
    def appendOutput(url : FileUrl) : Output[Byte] =
      new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile, true)))
  }

  /** Type class object for reading `Byte`s from `HttpUrl`s */
  implicit object HttpStreamByteReader extends StreamReader[HttpUrl, Byte] {
    def input(url : HttpUrl) : Input[Byte] =
      new ByteInput(new BufferedInputStream(url.javaConnection.getInputStream))
  }

  /** Type class object for reading `Byte`s from `HttpsUrl`s */
  implicit object HttpsStreamByteReader extends StreamReader[HttpsUrl, Byte] {
    def input(url : HttpsUrl) : Input[Byte] =
      new ByteInput(new BufferedInputStream(url.javaConnection.getInputStream))
  }

  implicit def stdoutWriter[Data] = new StreamWriter[Stdout[Data], Data] {
    override def doNotClose = true
    def output(stdout : Stdout[Data]) = stdout.output
  }

  implicit def stderrWriter[Data] = new StreamWriter[Stderr[Data], Data] {
    override def doNotClose = true
    def output(stderr : Stderr[Data]) = stderr.output
  }

  implicit def stdin[Data] = new StreamReader[Stdin[Data], Data] {
    override def doNotClose = true
    def input(stdin : Stdin[Data]) = stdin.input
  }

  implicit def urlCodec(s : String) = new {
    def urlEncode(implicit encoding : Encodings.Encoding = Encodings.`UTF-8`) =
      URLEncoder.encode(s, encoding.name)
    def urlDecode(implicit encoding : Encodings.Encoding = Encodings.`UTF-8`) =
      URLDecoder.decode(s, encoding.name)
  }


}
