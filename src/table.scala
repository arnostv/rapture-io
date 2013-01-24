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

import scala.collection.mutable._

trait Tabulation { this: BaseIo =>
  
  implicit class StringExtras(string: String) {
    private def doWrap(text: String, width: Int, handle: ((String, Int) => List[String])) =
      text.split("\n") map { line =>
        val words = line.split(" ") flatMap { w =>
          if(w.length > width) handle(w, width) else List(w)
        }
        val sb = new StringBuilder(words.head)
        var n = words.head.length
        for(w <- words.tail) {
          if(n + w.length + 1 > width) {
            sb.append("\n")
            n = 0
          } else {
            sb.append(" ")
            n += 1
          }
          sb.append(w)
          n += w.length
        }
        sb.toString
      } mkString "\n"

    def wrap(width: Int, handleLongWord: ((String, Int) => List[String]) = { (x, w) =>
        hyphenate(x, w) }) = doWrap(string, width, handleLongWord)

    def hyphenate(w : String, width : Int) : List[String] =
      (w.sliding(width - 1, width - 1).toList.reverse match {
        case h :: t => h :: t.map(_+"-")
        case z => z
      }).reverse

  }

  class RecordLayout(width: Int, records: (String, String)*) extends
      Tabulation[(String, String)](records.toList) {
    
    Column(_._1, alignment = Align.Right)
    Column(_._2.wrap(width))
  }

  class Tabulation[X](rows: List[X]) {

    private val cols = new ListBuffer[Column]

    def showTitle = cols.forall(_.title != null)

    object Align extends Enumeration {
      val Left, Right, Center = Value
    }

    def pad(w: Int, s: String, align: Align.Value) =
      if(s.length > w) (s.substring(0, w - 3)+"...") else {
        align match {
          case Align.Left => s+(" "*(w - s.length))
          case Align.Right => (" "*(w - s.length))+s
          case Align.Center => (" "*((w - s.length)/2))+s+(" "*((w - s.length + 1)/2))
        }
      }

    def columnPadding = 2

    case class Column(content: X => String, title: String = null,
        alignment: Align.Value = Align.Left) {
      
      cols += this
      
      val contents = rows.map(content).toArray
      
      lazy val width = contents.foldLeft(title.fromNull.getOrElse("").length) { (a, b) =>
        scala.math.max(a, b.length)
      }
    }

    def render() = {
      val sb = new StringBuffer
      
      if(showTitle) {
        
        for(c <- cols) {
          sb.append(pad(c.width, c.title.fromNull.getOrElse(""), c.alignment))
          sb.append(" "*columnPadding)
        }
        
        sb.append("\n")
        
        for(c <- cols) {
          sb.append(pad(c.width, "-"*c.title.fromNull.getOrElse("").length, c.alignment))
          sb.append(" "*columnPadding)
        }
        
        sb.append("\n")
      }
      
      for(i <- 0 until rows.length) {
        
        for(c <- cols) {
          sb.append(pad(c.width, c.contents(i), c.alignment))
          sb.append(" "*columnPadding)
        }

        sb.append("\n")
      }

      sb.toString
    }
  }
}
