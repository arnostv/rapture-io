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

import java.io._
import java.net._

/** Defines some extractors for retrieving data from character streams */
trait Extractors { this: Io =>

  /** Defines extractors for getting Scala XML from a String or Input[Char]. The stream extractor
    * simply collects the input into a String, then parses the String, so more efficient
    * implementation may be possible. .*/
  object Xml {
    
    def unapply(in: Input[Char]): Option[Seq[scala.xml.Node]] = try {
      val so = new StringOutput
      in > so
      unapply(so.buffer)
    } catch { case e: Exception => None }

    def unapply(in: String): Option[Seq[scala.xml.Node]] =
      try { Some(scala.xml.XML.loadString(in)) } catch { case e: Exception => None }
  }

  /** Defines extractors for parsing a JSON array from an Input[Char] or a String. */
  object JsonArray {
    
    def unapply(in: Input[Char]): Option[List[Any]] = try {
      val so = new StringOutput
      in > so
      unapply(so.buffer)
    } catch { case e: Exception => None }

    def unapply(in: String): Option[List[Any]] = try {
      scala.util.parsing.json.JSON.parseFull(in) flatMap { a =>
        if(a.isInstanceOf[List[_]]) Some(a.asInstanceOf[List[Any]]) else None
      }
    } catch { case e: Exception => None }
  }

  /** Defines extractors for parsing a JSON object from an Input[Char] or a String. */
  object JsonObject {

    def unapply(in: Input[Char]): Option[Map[String, Any]] = try {
      val so = new StringOutput
      in > so
      unapply(so.buffer)
    } catch { case e: Exception => None }

    def unapply(in: String): Option[Map[String, Any]] = try {
      scala.util.parsing.json.JSON.parseFull(in) flatMap { a =>
        if(a.isInstanceOf[Map[_, _]]) Some(a.asInstanceOf[Map[String, Any]]) else None
      }
    } catch { case e: Exception => None }
  }

}

