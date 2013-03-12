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

// Rewrite using actors
trait Logging { logging: BaseIo =>

  /** Basic logging functionality, introducing the concept of logging zones. Note that this is
    * almost certainly not as efficient as it ought to be, so use something else if efficiency
    * matters to you. */

  case class Zone(name: String)

  case class Level(level: Int, name: String)
  object Trace extends Level(6, "trace")
  object Debug extends Level(5, "debug")
  object Info extends Level(4, "info")
  object Warn extends Level(3, "warn")
  object Error extends Level(2, "error")
  object Fatal extends Level(1, "fatal")

  trait Logger { def log(msg: String, level: Level, zone: Zone) }

  case class FileLogger(file: FileUrl) extends Logger {
    def log(msg: String, level: Level, zone: Zone) = (msg+"\n") >> file
  }

  case object StdoutLogger extends Logger {
    def log(msg: String, level: Level, zone: Zone) = println(msg)
  }

  object log {

    implicit val zone = Zone("logger")
    
    var listeners: List[(Logger, Level, Map[Zone, Level])] = Nil

    val df = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    var dateString = ""
    var dateCreated = 0L

    def listen(logger: Logger, level: Level = Info, spec: Map[Zone, Level] = Map()): Unit = {
      info("Registering listener")
      listeners = (logger, level, spec) :: listeners
    }

    def unlisten(logger: Logger) = {
      listeners = listeners.filter(_._1 != logger)
    }

    @inline def trace(msg: => String)(implicit zone: Zone) =
      log(Trace, zone, msg)
    
    @inline def debug(msg: => String)(implicit zone: Zone) =
      log(Debug, zone, msg)
    
    @inline def info(msg: => String)(implicit zone: Zone) =
      log(Info, zone, msg)
    
    @inline def warn(msg: => String)(implicit zone: Zone) =
      log(Warn, zone, msg)
    
    @inline def error(msg: => String)(implicit zone: Zone) =
      log(Error, zone, msg)
    
    @inline def fatal(msg: => String)(implicit zone: Zone) =
      log(Fatal, zone, msg)
    
    @inline def exception(e: => Throwable)(implicit zone: Zone) = {
      log(Error, zone, e.toString)
      log(Debug, zone, "    "+e.getStackTrace.mkString("\n    "))
    }

    private def log(level: Level, zone: Zone, msg: String): Unit = {
      val time: Long = System.currentTimeMillis
      // Ensures the date is only formatted when it changes
      if(time != dateCreated) {
        dateString = df.format(time)
        dateCreated = time
      }
      val m = if(msg == null) "null" else msg
      val ln = m.replaceAll("\n", "\n                                       ")
      val formattedMsg = "%1$-23s %2$-5s %3$-8s %4$s".format(dateString, level.name, zone.name, ln)
      
      for((lgr, lvl, spec) <- listeners if spec.getOrElse(zone, lvl).level >= level.level) {
        try lgr.log(formattedMsg, level, zone) catch {
          case e: Exception =>
        }
      }
    }
  }

  class TcpLogServer(port: Int) {

    implicit val enc = Encodings.`UTF-8`

    private def readLevel(s: String) = s match {
      case "debug" => Debug
      case "info" => Info
      case "warn" => Warn
      case "error" => Error
      case "fatal" => Fatal
      case _ => Trace
    }

    def await(): Unit = {
      tcpHandle[String](port) { case (in, out) =>
        val logger = new Logger {
          def log(msg: String, level: Level, zone: Zone) = {
            out.write(level.level+msg)
            out.flush()
          }
        }
        val spec = in.read().get
        val level = readLevel(spec.split("&")(0))
        val zs = (spec.split("&").tail map { x =>
          val q = x.split("=")
          Zone(q(0)) -> readLevel(q(1))
        }).toMap
        log.listen(logger, level, zs)
        try in.slurp() catch {
          case e: Exception => ()
        }
        log.unlisten(logger)
      }
    }
  }
}
