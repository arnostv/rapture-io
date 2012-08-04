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

/** Provides references to standard character encodings provided by Java. Encodings are represented
  * by instances of the Encoding case class, which is a simple wrapper over a String of the
  * encoding's name. Several standard encodings are provided and identified by the encoding's
  * canonical name for the avoidance of ambiguity. These instances will typically require escaping
  * with backticks in order to be referenced, however type safety will be ensured. */
object Encodings extends Lookup[String] {
  
  type Item = Encoding
  case class Encoding(name : String) extends AutoAppend { def index = name }
  
  val `US-ASCII` = Encoding("US-ASCII")
  val `windows-1250` = Encoding("windows-1250")
  val `windows-1251` = Encoding("windows-1251")
  val `windows-1252` = Encoding("windows-1252")
  val `windows-1253` = Encoding("windows-1253")
  val `windows-1254` = Encoding("windows-1254")
  val `windows-1257` = Encoding("windows-1257")
  val `ISO-8859-1` = Encoding("ISO-8859-1")
  val `ISO-8859-2` = Encoding("ISO-8859-2")
  val `ISO-8859-4` = Encoding("ISO-8859-4")
  val `ISO-8859-5` = Encoding("ISO-8859-5")
  val `ISO-8859-7` = Encoding("ISO-8859-7")
  val `ISO-8859-9` = Encoding("ISO-8859-9")
  val `ISO-8859-13` = Encoding("ISO-8859-13")
  val `ISO-8859-15` = Encoding("ISO-8859-15")
  val `KOI8-R` = Encoding("KOI8-R")
  val `UTF-8` = Encoding("UTF-8")
  val `UTF-16` = Encoding("UTF-16")
  val `UTF-16BE` = Encoding("UTF-16BE")
  val `UTF-16LE ` = Encoding("UTF-16LE")

  /** The default file system encoding for this system */
  lazy val Default = lookup(System.getProperty("file.encoding"))

}
