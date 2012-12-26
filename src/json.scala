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

import language.dynamics

/** Some useful JSON shortcuts */
trait JsonExtraction { this: Io =>

  /** Represents a JSON parser implementation which is used throughout this library */
  trait JsonParser {
    def parse(s: String): Option[Any]
  }
  
  /** The default JSON parser implementation */
  implicit val ScalaJsonParser = new JsonParser {
    
    import scala.util.parsing.json._
    
    def parse(s: String): Option[Any] = JSON.parseFull(s)
  }

  /** Provides support for JSON literals, in the form json" { } " or json""" { } """. Interpolation
    * is used to substitute variable names into the JSON, and to extract values from a JSON string.
    */
  @inline implicit class JsonStrings(sc: StringContext)(implicit jp: JsonParser) extends {
    object json {
      /** Creates a new interpolated JSON object. */
      def apply(exprs: Any*): ![Exception, Json] = except {
        val sb = new StringBuilder
        val textParts = sc.parts.iterator
        val expressions = exprs.iterator
        sb.append(textParts.next())
        while(textParts.hasNext) {
          sb.append(expressions.next match {
            case s: String => "\""+s+"\""
            case a => a.toString
          })
          sb.append(textParts.next)
        }
        Json.parse(sb.toString)(jp)
      }

      /** Extracts values in the structure specified from parsed JSON.  Each element in the JSON
        * structure is compared with the JSON to extract from.  Broadly speaking, elements whose
        * values are specified in the extractor must match, whereas variable elements appearing
        * in the extractor must exist. Lists may not appear in the extractor. */
      def unapplySeq(json: Json): Option[Seq[Json]] = try {
        var paths: List[SimplePath] = Nil
        def extract(struct: Any, path: SimplePath): Unit =
          struct match {
            case d: Double =>
              if(json.extract(path).get[Double](JsonExtractor.doubleJsonExtractor) != d)
                throw new Exception("Value doesn't match")
            case s: String =>
              if(json.extract(path).get[String](JsonExtractor.stringJsonExtractor) != s)
                throw new Exception("Value doesn't match")
            case m: Map[_, _] => m foreach {
              case (k, v) =>
                if(v == null) paths ::= (path / k.asInstanceOf[String])
                else extract(v, path / k.asInstanceOf[String])
            }
            case a: List[_] => ()
              // Emit an exception if attempting to extract on lists
          }
        extract(jp.parse(sc.parts.mkString("null")).get, ^)
        val extracts = paths.reverse.map(json.extract)
        if(extracts.exists(_.json == null)) None else Some(extracts)
      } catch { case e: Exception => None }
    }
  }

  /** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
    * pretty printer. */
  object Json {

    /** Parses a string containing JSON into a `Json` object */
    def parse(s: String)(implicit jp: JsonParser): Json = new Json(jp.parse(s).get)

    /** Wraps a map into a JSON object */
    def apply(map: Map[String, Any]): Json = new Json(map)

    /** Wraps a list into a JSON array */
    def apply(list: List[Any]): Json = new Json(list)

    def unapply(json: Any): Option[Json] = Some(new Json(json))

    /** Formats the JSON object for multi-line readability. */
    def format(json: Option[Any], ln: Int): String = {
      val indent = " "*ln
      json match {
        case Some(o: Map[_, _]) =>
          List("{", o.keys map { k => indent+" "+"\""+k+"\": "+format(o.get(k), ln + 1) } mkString
              ",\n", indent+"}").mkString("\n")
        case Some(a: List[_]) =>
          List("[", a map { v => indent+" "+format(Some(v), ln + 1) } mkString(",\n"),
              indent+"]") mkString "\n"
        case Some(s: String) =>
          "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\n", "\\\\n").replaceAll("\"",
              "\\\\\"")+"\""
        case Some(n: Int) => n.toString
        case Some(n: Number) => n.toString
        case Some(v: Boolean) => if(v) "true" else "false"
        case None => "null"
        case _ => "undefined"
      }
    }
    
  }

  /** Companion object for JsonExtractor type. Defines very simple extractor methods for different
    * types which may be contained within. */
  object JsonExtractor {
    
    implicit val noopExtractor = new JsonExtractor[Json](x => new Json(x))
    implicit val stringJsonExtractor = new JsonExtractor[String](_.asInstanceOf[String])
    implicit val doubleJsonExtractor = new JsonExtractor[Double](_.asInstanceOf[Double])
    implicit val intJsonExtractor = new JsonExtractor[Int](_.asInstanceOf[Double].toInt)
    implicit val longJsonExtractor = new JsonExtractor[Long](_.asInstanceOf[Double].toLong)
    implicit val booleanJsonExtractor = new JsonExtractor[Boolean](_.asInstanceOf[Boolean])
    
    implicit def listJsonExtractor[T: JsonExtractor] =
      new JsonExtractor[List[T]](_.asInstanceOf[List[Any]].map(implicitly[JsonExtractor[T]].cast))
    
    implicit def mapJsonExtractor[T: JsonExtractor] =
      new JsonExtractor[Map[String, T]](_.asInstanceOf[Map[String, Any]].mapValues(
          implicitly[JsonExtractor[T]].cast))
  }

  @annotation.implicitNotFound("Cannot extract type ${T} from JSON.")
  class JsonExtractor[T](val cast: Any => T)
  implicit val nullExtractor: JsonExtractor[Json] = new JsonExtractor[Json](x => new Json(x))

  class Json(private[JsonExtraction] val json: Any) extends Dynamic {

    /** Assumes the Json object is wrapping a List, and extracts the `i`th element from the list */
    def apply(i: Int): Json =
      new Json(if(json == null) null else json.asInstanceOf[List[Any]].apply(i))
   
    /** Combines a `selectDynamic` and an `apply`.  This is necessary due to the way dynamic
      * application is expanded. */
    def applyDynamic(key: String)(i: Int): Json = selectDynamic(key).apply(i)
    
    /** Navigates the JSON using the `SimplePath` parameter, and returns the element at that
      * position in the tree. */
    def extract(sp: SimplePath): Json =
      if(sp == ^) this else selectDynamic(sp.head).extract(sp.tail)
    
    /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
    def selectDynamic(key: String): Json =
      new Json(if(json == null) null else json.asInstanceOf[Map[String, Any]].get(key).getOrElse(
          null))
    
    /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
    def get[T](implicit jsonExtractor: JsonExtractor[T]): ![Exception, T] =
      except(jsonExtractor.cast(json))

    /** Assumes the Json object is wrapping a List, and returns the length */
    def length = json.asInstanceOf[List[Json]].length

    /** Assumes the Json object is wrapping a List, and returns an iterator over the list */
    def iterator: Iterator[Json] = json.asInstanceOf[List[Json]].iterator

    override def toString = Json.format(Some(json), 0)
  }
}
