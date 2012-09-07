package rapture.io

import scala.util.parsing.json._

/** Pretty printer for formatting JSON for readability */
object JsonPrinter {

  def format(json: Option[Any], ln: Int): String = {
    val indent = " "*ln
    json match {
      case Some(o: JSONObject) => {
        List("{",
          o.obj.keys.map(key => indent+" "+"\""+key+"\": "+
              format(o.obj.get(key), ln + 1)).mkString(",\n"),
          indent+"}").mkString("\n")
      }
      case Some(a: JSONArray) => {
        List("[",
          a.list.map(v => indent+" "+format(Some(v), ln + 1)).mkString(",\n"),
          indent+"]").mkString("\n")
      }
      case Some(s: String) =>
        "\""+s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+
            "\""
      case Some(n: Number) => n.toString
      case Some(v: Boolean) => if(v) "true" else "false"
      case None => "null"
      case _ => "undefined"
    }
  }

}


