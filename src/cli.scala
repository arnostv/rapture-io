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

import scala.collection.immutable.{Queue, ListMap}
import scala.collection.mutable.HashMap

trait CommandLine { this: BaseIo =>

  trait CliMessages {

    def unparsableMsg(format: String, string: String) =
      "Could not understand "+format+" format parameter '"+string+"'"
    
    def unspecifiedMsg(name: String) =
      "The parameter --"+name+" has not been specified."

    def unspecifiedValueMsg(value: String, parameter: String) =
      "Value <"+value+"> has not been specified for parameter --"+parameter
    
    def unknownOptionMsg(option: String) =
      "Unknown option: --"+option
    
    def unknownOption1Msg(option: String) =
      "Unknown option: -"+option
  }

  trait Cli extends CliMessages { cli =>

    case class CliException(msg: String) extends RuntimeException(msg)

    abstract class CliParser[T](val name: String) {
      def completions: Completion
      def parse(s: String): Option[T]
    }

    implicit def optParser[T](implicit cp: CliParser[T]): CliParser[Option[T]] =
      new CliParser[Option[T]](cp.name) {
        def parse(s: String): Option[Option[T]] = Some(cp.parse(s))
        def completions = cp.completions
      }

    implicit val StringParser = new CliParser[String]("string") {
      def parse(s: String): Option[String] = Some(s)
      def completions = NoCompletions
    }

    implicit val DoubleParser = new CliParser[Double]("double") {
      def parse(s: String): Option[Double] = try { Some(s.toDouble) } catch {
        case e: NumberFormatException => None
      }
      def completions = NoCompletions
    }

    implicit val IntParser = new CliParser[Int]("integer") {
      def parse(s: String): Option[Int] = try { Some(s.toInt) } catch {
        case e: NumberFormatException => None
      }
      def completions = NoCompletions
    }

    implicit val BooleanParser = new CliParser[Boolean]("boolean") {
      
      private val yes = List("on", "true", "t", "1", "yes", "y")
      private val no = List("off", "false", "f", "0", "no", "n")
      
      def parse(s: String): Option[Boolean] =
        if(yes contains s.toLowerCase) Some(true)
        else if(no contains s.toLowerCase) Some(false)
        else None
      
      def completions = Completions(List("true", "false"))
    }

    trait Completion

    case class Completions(cs: List[String]) extends Completion {
      override def toString = cs.mkString("(", "+", ")")
    }

    object AllFiles extends Completion { override def toString = "_files" }
    object NoCompletions extends Completion { override def toString = "()" }

    class Opts {
      
      private var args: List[String] = Nil
      
      def read(as: List[String]) = args = as
      
      abstract class Opt[T: CliParser] {
        def apply(): T
        def value: Option[String]
        def longName: String
        def shortName: Char
        def summary: String
        def specifiedCompletions: List[String]
        def completions: Completion =
          if(specifiedCompletions != null) Completions(specifiedCompletions) else implicitly[CliParser[T]].completions
        
        def zshCompletions =
          if(shortName == ' ') List("(--"+longName+")--"+longName+"["+summary.replaceAll(" ", "+")+"]")
          else List("(--"+longName+"+-"+shortName+")--"+longName+"["+summary.replaceAll(" ", "+")+"]",
              "(--"+longName+"+-"+shortName+")-"+shortName+"["+summary.replaceAll(" ", "+")+"]")
      }
      
      val opts: HashMap[String, Opt[_]] = HashMap[String, Opt[_]]()
      private val shortOpts: HashMap[Char, Opt[_]] = HashMap[Char, Opt[_]]()

      def zshCompletions = ((opts.toList flatMap { case (k, v) => v.zshCompletions }) :::
          List("*:+:()")).mkString("\n")

      def optParam[T: CliParser](ln: String, help: String, valueName: String, sn: Char = ' ',
          _completions: List[String] = null) =
        new Opt[Option[T]] {
          
          def value = Some(valueName)
          def longName = ln
          def shortName = sn
          def summary = help
          def specifiedCompletions = _completions
          
          def apply(): Option[T] = {
            val parser = implicitly[CliParser[T]]
            parsing._1.get(opts(longName)) map { x =>
              parser.parse(x.get).getOrElse(throw CliException(unparsableMsg(parser.name, x.get)))
            }
          }
          
          opts(longName) = this
          if(shortName != ' ') shortOpts(shortName) = this
          
          override def zshCompletions = super.zshCompletions map { ln =>
            ln+":"+value.get.replaceAll(" ", "+")+":"+completions.toString
          }
        
        }

      def param[T: CliParser](ln: String, help: String, valueName: String,
          sn: Char = ' ', _completions: List[String] = null) = new Opt[T] {
       
        def value = Some(valueName)
        def longName = ln
        def shortName = sn
        def summary = help
        def specifiedCompletions = _completions

        def apply(): T = implicitly[CliParser[T]].parse(parsing._1(this).getOrElse(throw CliException(unspecifiedMsg(longName)))).getOrElse(throw CliException(unparsableMsg(implicitly[CliParser[T]].name, parsing._1(this).get)))
        
        opts(longName) = this
        if(shortName != ' ') shortOpts(shortName) = this
        
        override def zshCompletions = super.zshCompletions map { ln =>
          ln+":"+value.get.replaceAll(" ", "+")+":"+completions.toString.replaceAll(" ", "+")
        }
      
      }

      def flag(ln: String, help: String, sn: Char = ' ') = new Opt[Boolean] {

        def value = None
        def longName = ln
        def shortName = sn
        def summary = help
        def specifiedCompletions = null

        def apply(): Boolean = parsing._1.contains(opts(longName))
        
        opts(longName) = this
        if(shortName != ' ') shortOpts(shortName) = this
      }

      def remaining = parsing._2.reverse

      // FIXME: When making this a (lazy) val, it seems to get cached between multiple runs of
      // nailgun, rendering it useless. Presumably this is because it's declared as final.
      private def parsing = {
        val ps: HashMap[Opt[_], Option[String]] = HashMap()
        var queue: Queue[Opt[_]] = Queue()
        var remainder: List[String] = Nil
        
        for(a <- args) {
          if(a startsWith "--") {
            if(a.indexOf("=") == -1) {
              val o = opts.getOrElse(a.substring(2),
                  throw CliException(unknownOptionMsg(a.substring(2))))
              if(o.value.isDefined) queue = queue enqueue o
              else ps(o) = None
            } else {
              val o = opts.getOrElse(a.substring(2, a.indexOf("=")),
                  throw CliException(unknownOptionMsg(a.substring(2))))
              ps(o) = Some(a.substring(a.indexOf("=") + 1))
            }
          } else if(a startsWith "-") {
            if(!queue.isEmpty) {
              val o = queue.dequeue._1
              throw CliException(unspecifiedValueMsg(o.value.get, o.longName))
            }
            a.substring(1).toList foreach { so =>
              val o = shortOpts.getOrElse(so, throw CliException(unknownOption1Msg(so.toString)))
              if(o.value.isDefined) queue = queue enqueue o
              else ps(o) = None
            }
          } else {
            if(queue.isEmpty) {
              remainder ::= a
            } else {
              val (o, q) = queue.dequeue
              queue = q
              ps(o) = Some(a)
            }
          }
        }
        queue foreach { qi =>
          throw CliException(unspecifiedValueMsg(qi.value.get, qi.longName))
        }
        (ps, remainder)
      }


    }
  }

  trait CliApp extends Cli {
    
    class Action[T <: Opts](val opts: T, val summary: String, val blk: T => String,
        val hidden: Boolean) {
      def zshCompletions = opts.zshCompletions
      def perform(args: List[String]): String = {
        opts.read(args)
        blk(opts)
      }
    }

    private var defaultCmd: Option[Action[_]] = None

  }

  trait SubcommandApp extends CliApp {

    var subcommands: ListMap[String, Action[_]] = ListMap()

    def subcommand[T <: Opts](cmd: String, summary: String, opts: T, hidden: Boolean = false)(block: T => String) = {
      subcommands += cmd -> new Action[T](opts, summary, block, hidden)
    }

    private var defaultCmd: () => String = null
    
    def defaultCommand(block: => String) = defaultCmd = () => block

    def main(args: Array[String]): Unit = {
      if(args.isEmpty) println("Please specify a command.")
      else try {
        subcommands.get(args.head) map { a => println(a.perform(args.toList.tail)) } getOrElse
            println(defaultCmd())
      } catch {
        case CliException(msg) => println(msg)
      }
    }
  
    subcommand("zsh-completion", "Lookup zsh options", new Opts {
      val subcommand = optParam[String]("subcommand", "Get the options for this subcommand",
        "subcommand", 's')
    }, hidden = true) { opts =>
      opts.subcommand() match {
        case Some(sc) =>
          subcommands.get(sc) match {
            case Some(act) =>
              act.zshCompletions
            case None =>
              ""
          }
        case None =>
          subcommands.filter(!_._2.hidden) map { case (k, v) =>
            k+"["+v.summary.replaceAll(" ", "+")+"]"
          } mkString "\n"
      }
    }
  }
}
