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

import scala.util.parsing.json._

/** Some useful JSON shortcuts */
trait JsonExtraction { this: Io =>

  implicit class JsonStrings(sc: StringContext) extends {
    object json {
      def apply(exprs: Any*): **[Json] = {
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
        **(Json.parse(sb.toString))
      }

      def unapplySeq(json: Json): Option[Seq[Json]] = try {
        var extracted: List[SimplePath] = Nil
        def extract(struct: Any, path: SimplePath): Unit =
          struct match {
            case d: Double => ()
            case s: String => ()
            case m: Map[_, _] => m foreach {
              case (k, v) =>
                if(v == null) extracted ::= (path / k.asInstanceOf[String])
                else extract(v, path / k.asInstanceOf[String])
            }
            case a: List[_] => ()
              // Emit an exception if attempting to extract on lists
          }
        extract(JSON.parseFull(sc.parts.mkString("null")).get, ^)
        Some(extracted.reverse.map(json.extract))
      } catch { case e: Exception => None }
    }
  }

  object Json {

    def parse(s: String): Json = new Json(JSON.parseFull(s).get)

    def apply(map: Map[String, Any]): Json = new Json(map)
    def apply(list: List[Any]): Json = new Json(list)

    def unapply(json: Any): Option[Json] = Some(new Json(json))

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
          "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+
              "\""
        case Some(n: Int) => n.toString
        case Some(n: Number) => n.toString
        case Some(v: Boolean) => if(v) "true" else "false"
        case None => "null"
        case _ => "undefined"
      }
    }
    
  }

  object JsonExtractor {
    implicit val stringJsonExtractor = new JsonExtractor[String](_.asInstanceOf[String])
    implicit val doubleJsonExtractor = new JsonExtractor[Double](_.asInstanceOf[Double])
    implicit val intJsonExtractor = new JsonExtractor[Int](_.asInstanceOf[Double].toInt)
    implicit val longJsonExtractor = new JsonExtractor[Long](_.asInstanceOf[Double].toLong)
    implicit val booleanJsonExtractor = new JsonExtractor[Boolean](_.asInstanceOf[Boolean])
    implicit def listJsonExtractor[T] = new JsonExtractor[List[T]](_.asInstanceOf[List[T]])
    implicit def mapJsonExtractor[T] = new JsonExtractor[Map[String, T]](_.asInstanceOf[Map[String, T]])
  }

  @annotation.implicitNotFound("Cannot extract type ${T} from JSON.")
  class JsonExtractor[T](val cast: Any => T)
  implicit val nullExtractor: JsonExtractor[Json] = new JsonExtractor[Json](x => new Json(x))

  class Json(json: Any) extends Dynamic {

    def apply(i: Int): Json = new Json(json.asInstanceOf[List[Any]].apply(i))
    
    def applyDynamic(key: String)(i: Int) =
      new Json(json.asInstanceOf[Map[String, Any]].apply(key).asInstanceOf[List[Any]].apply(i))
    
    def extract(sp: SimplePath): Json = if(sp == ^) this else selectDynamic(sp.head).extract(sp.tail)
    
    def selectDynamic(key: String): Json = new Json(json.asInstanceOf[Map[String, Any]].apply(key))
    //def selectDynamic[T](key: String)(implicit je: JsonExtractor[T]): T =
    //  new Json(json.asInstanceOf[Map[String, Any]].apply(key)).asInstanceOf[T]
    
    def get[T](implicit jsonExtractor: JsonExtractor[T]): T = jsonExtractor.cast(json)

    def length = json.asInstanceOf[List[Json]].length
    def iterator: Iterator[Json] = json.asInstanceOf[List[Json]].iterator

    override def toString = Json.format(Some(json), 0)
  }

}
