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

trait Processes { this: BaseIo =>

  private val runtime = Runtime.getRuntime

  class Proc(val process: Process)

  implicit class ExecStrings(sc: StringContext) extends {
    object sh {
      def apply(exprs: String*): ![Exception, Proc] = {
        val sb = new StringBuilder
        val textParts = sc.parts.iterator
        val expressions = exprs.iterator
        sb.append(textParts.next())
        while(textParts.hasNext) {
          sb.append(expressions.next())
          sb.append(textParts.next)
        }
        // FIXME: Parse command, rather than assuming split on spaces
        except(new Proc(runtime.exec(sb.toString.split(" "))))
      }
    }
  }

  implicit def procIsReadable(implicit enc: Encoding): StreamReader[Proc, Char] =
    new StreamReader[Proc, Char] {
      def input(proc: Proc): ![Exception, Input[Char]] =
        except(inputStreamCharBuilder.input(proc.process.getInputStream))
    }

  /** Convenience method for forking a block of code to a new thread */
  def fork(blk: => Unit): Thread = {
    val th = new Thread {
      override def run() = {
        blk
        join()
      }
    }
    th.start()
    th
  }
}
