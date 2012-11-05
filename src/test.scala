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

    val getString = test { Json.parse(src).string[String] } yields "Hello World"
    val getInt = test { Json.parse(src).int[Int] } yields 78
    val navigateObj = test { Json.parse(src).obj.a[String] } yields "A"
    val getList = test { Json.parse(src).array[List[Int]] } yields List(1, 2, 3)
  }

  val mime = new Suite("mime.scala") {
    val getMimeType = test { MimeTypes.fromString("text/plain") } yields Some(MimeTypes.`text/plain`)
    val getExtensions = test { MimeTypes.`application/xml`.extensions } satisfies (_ contains "xml")
    val lookupFromExtension = test { MimeTypes.extension("xsl") } satisfies (_ contains MimeTypes.`application/xml`)
  }

  val paths = new Suite("path.scala") {
    val pathToString = test { (^ / "foo" / "bar").toString } yields "/foo/bar"
    val terminatedPath = test { (^ / "foo" / "bar" / $).toString } yields "/foo/bar/"
    val getParent = test { (^ / "foo" / "bar").parent } satisfies (_ == ^ / "foo")
    val drop = test { (^ / "foo" / "bar" / "baz").drop(2) } satisfies (_ == ^ / "foo")
    val take = test { (^ / "foo" / "bar" / "baz").take(2) } satisfies (_ == ^ / "foo" / "bar")
    val pathAppend = test { (^ / "foo" / "bar") + (^ / "baz") } satisfies (_ == ^ / "foo" / "bar" / "baz")
  }

  val digest = new Suite("digest.scala") {
    
    val md5Sum = test {
      "Hello World".getBytes("UTF-8").md5Sum().toLowerCase
    } yields "e59ff97941044f85df5297e1c302d260"
    
  }

  val streams = new Suite("io.scala") {
    val download = test {
      val src = Http / "www.propensive.com" / "downloads" / "aa0d5d6739177b2bbb4c508a3014d8b0"
      val dest = File / "tmp" / "aa0d5d6739177b2bbb4c508a3014d8b0"
      src > dest
      dest.md5Sum().toLowerCase
    } yields "aa0d5d6739177b2bbb4c508a3014d8b0"
  }

  /*val base64 = new Suite("base64.scala") {
    val encode = test { Base64.encode("Hello World") } yields "SGVsbG8gV29ybGQK"
    val encode2 = test { Base64.encode("Hello World!") } yields "SGVsbG8gV29ybGQhCg=="
    val encode3 = test { Base64.encode("Hello World!!") } yields "SGVsbG8gV29ybGQhIQo="
  }*/

}
