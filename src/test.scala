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

package tests.rapture.io

import rapture.test._
import rapture.io._

object Tests extends TestingApplication {

  val generators = new Suite("generators.scala") {

    val stopOnZero = test {
      // Set up an artificial sequence
      var current = 5
      def next() = {
        current -= 1
        current
      }

      // Multiply all the values together
      Generator.non(0)(next()).foldLeft(1)(_*_)
    
    } yields 24

    val stopOnNull = test {
      var current = 5
      def next(): Option[Int] = {
        current -= 1
        if(current == 0) null else Some(current)
      }

      // Multiply all the values together
      Generator.nonNull(next()).foldLeft(1)(_*_.get)
    } yields 24
  }

  val ip = new Suite("ip.scala") {
    val parseIp = test { Ip4.parse("10.11.12.13") } yields Ip4(10, 11, 12, 13)
    val readIpFromLong = test { Ip4.fromLong(3232243145L) } yields Ip4(192, 168, 29, 201)
    val isPrivate1 = test { Ip4(10, 7, 95, 28).isPrivate } yields true
    val isPrivate2 = test { Ip4(192, 168, 99, 107).isPrivate } yields true
    val isPrivate3 = test { Ip4(172, 16, 152, 12).isPrivate } yields true
    val isNotPrivate = test { Ip4(106, 184, 221, 34).isPrivate } yields false
    val parseSubnet = test { Subnet.parse("18.0.0.0/8") } yields new Subnet(Ip4(18, 0, 0, 0), 8)
    val isInSubnet = test { Ip4.parse("18.19.20.21") in Subnet.parse("18.0.0.0/8") } yields true
  }

  val mime = new Suite("mime.scala") {
    val getMimeType =
      test { MimeTypes.fromString("text/plain") } yields Some(MimeTypes.`text/plain`)
    
    val getExtensions = test { MimeTypes.`application/xml`.extensions } satisfies (_ contains "xml")
    
    val lookupFromExtension =
      test { MimeTypes.extension("xsl") } satisfies (_ contains MimeTypes.`application/xml`)
  }

  val paths = new Suite("path.scala") {
    val pathToString = test { (^ / "foo" / "bar").toString } yields "/foo/bar"
    val terminatedPath = test { (^ / "foo" / "bar" / $).toString } yields "/foo/bar/"
    val getParent = test { (^ / "foo" / "bar").parent } satisfies (_ == ^ / "foo")
    val drop = test { (^ / "foo" / "bar" / "baz").drop(2) } satisfies (_ == ^ / "foo")
    val take = test { (^ / "foo" / "bar" / "baz").take(2) } satisfies (_ == ^ / "foo" / "bar")
    val pathAppend = test { ((^ / "foo" / "bar") + (^ / "baz")).toString } yields "/foo/bar/baz"
    val head = test { (^ / "foo" / "bar" / "baz").head } yields "foo"
    val tail = test { (^ / "foo" / "bar" / "baz").tail } yields (^ / "bar" / "baz")
    val last = test { (^ / "foo" / "bar" / "baz").last } yields "baz"
    val init = test { (^ / "foo" / "bar" / "baz").init } yields (^ / "foo" / "bar")
  
    val extract = test {
      val path = ^ / "foo" / "bar"
      val ^ / foo / bar = path
      foo -> bar
    } yields ("foo", "bar")
  }

  val digest = new Suite("digest.scala") {
    
    val md5Sum = test {
      val q = "Hello World".getBytes("UTF-8").md5Sum().toLowerCase
    } yields "e59ff97941044f85df5297e1c302d260"
    
  }

  val streams = new Suite("io.scala") {
    val download = test {
      val src = Http / "rapture.io" / "download" / "aa0d5d6739177b2bbb4c508a3014d8b0"
      val dest = File / "tmp" / "aa0d5d6739177b2bbb4c508a3014d8b0"
      src > dest
      dest.md5Sum().toLowerCase
    } yields "aa0d5d6739177b2bbb4c508a3014d8b0"
  }

  val base64 = new Suite("base64.scala") {
    implicit val encoding = Encodings.`UTF-8`
    val encode = test { Base64.encode("Hello World") } yields "SGVsbG8gV29ybGQ"
    val encode2 = test { Base64.encode("Hello World!") } yields "SGVsbG8gV29ybGQh"
    val encode3 = test { Base64.encode("Hello World!!") } yields "SGVsbG8gV29ybGQhIQ"
  }

  val linking = new Suite("links.scala") {
    val link1 = test { (^ link ^).toString } yields "."
    val link2 = test { (^ link ^ / "foo").toString } yields "foo"
    val link3 = test { (^ link ^ / "foo" / "bar").toString } yields "foo/bar"
    val link4 = test { (^ / "foo" link ^).toString } yields "/"
    val link5 = test { (^ / "foo" / "bar" link ^).toString } yields "../"
    val link6 = test { (^ / "foo" / "bar" link ^ / "foo").toString } yields "../foo"
    val link7 = test { (^ / "foo" / "bar" link ^ / "foo" / "bar").toString } yields "."
    val link8 = test { (^ / "baz" link ^ / "foo").toString } yields "foo"
    val link9 = test { (^ / "baz" / "quux" link ^ / "foo").toString } yields "../foo"
    val link10 = test { (^ / "baz" link ^ / "foo" / "bar").toString } yields "foo/bar"
    val link11 = test { (^ / "baz" / "quux" link ^ / "foo" / "bar").toString } yields "../foo/bar"
    val link12 = test { (^ / "bar" link ^ / "foo" / "bar").toString } yields "foo/bar"
    val link13 = test { (^ / "baz" / "quux" link ^ / "quux").toString } yields "../quux"
    
    val link14 =
      test { (^ / "foo" / "baz" / "quux" link ^ / "foo" / "quux").toString } yields "../quux"
  }

  val strings = new Suite("strings") {
    val httpUrl = test { (Http / "rapture.io").toString } yields "http://rapture.io/"
    val httpsUrl = test { (Https / "rapture.io").toString } yields "https://rapture.io/"
    val httpUrl2 = test { (Http / "rapture.io" / $).toString } yields "http://rapture.io/"
    
    val httpUrl3 =
      test { (Http / "rapture.io" / "foo" / "bar").toString } yields "http://rapture.io/foo/bar"
    
    val httpUrl4 =
      test { (Http./("rapture.io", 999) / "foo").toString } yields "http://rapture.io:999/foo"
    
    val fileUrl = test { (File / "foo" / "bar").toString } yields "file:///foo/bar"
    val fileRoot = test { (File / $).toString } yields "file:///"
    val path = test { ^.toString } yields "/"
    val path2 = test { (^ / "foo").toString } yields "/foo"
    val path3 = test { (^ / "foo" / "bar").toString } yields "/foo/bar"
  }

  val json = new Suite("json.scala") {
    val src = """
      {
        "string": "Hello World",
        "int": 78,
        "array": [1, 2, 3],
        "obj": {
          "a": "A",
          "b": "B",
          "c": "C"
        }
      }
    """

    val getString = test { Json.parse(src).string.get[String] } yields "Hello World"
    val getInt = test { Json.parse(src).int.get[Int] } yields 78
    val navigateObj = test { Json.parse(src).obj.a.get[String] } yields "A"
    val getList = test { Json.parse(src).array.get[List[Int]] } yields List(1, 2, 3)
    
    val extract1 = test {
      val json""" { "string": $x } """ = Json.parse(src)
      x.get[String]
    } yields "Hello World"
    
    val extract2 = test {
      val json""" { "int": $y } """ = Json.parse(src)
      y.get[Int]
    } yields 78
    
    val extract3 = test {
      val json""" { "array": $z } """ = Json.parse(src)
      z.get[List[Int]]
    } yields List(1, 2, 3)
    
    val extract4 = test({
      val json""" { "array": $z } """ = Json.parse(src)
      z.get[String]
    }).throws[Exception]
    
    val extract5 = test({
      val json""" { "foo": $x } """ = Json.parse(src)
      x
    }).throws[Exception]

    val extract6 = test {
      val json""" { "obj": { "b": $x } } """ = Json.parse(src)
      x.get[String]
    } yields "B"

    val extract7 = test {
      val json""" { "obj": { "a": "A", "b": $x } } """ = Json.parse(src)
      x.get[String]
    } yields "B"

    val extract8 = test({
      val json""" { "obj": { "a": "C", "b": $x } } """ = Json.parse(src)
      x.get[String]
    }).throws[Exception]
  }

}
