/**************************************************************************************************
Rapture I/O Library
Version 0.7.0

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

import language.implicitConversions

import scala.collection.mutable.WrappedArray

import java.io._
import java.net._

/** Defines generalised classes for handling hierarchical path structures and linking between them.
  * This forms the basis for hierarchical URLs. */
trait Paths { this: Io =>

  val $ = ""

  type AfterPath = Map[Char, (String, Double)]

  object / {
    def unapply(p: SimplePath): Option[(SimplePath, String)] =
      if(p.isRoot) None else Some((p.tail, p.head))
  }

  /** Represents an absolute (i.e. relative to a canonical base) path. */
  abstract class AbsolutePath[+PathType <: AbsolutePath[PathType]](elements: Seq[String], afterPath: AfterPath)
      extends Path[PathType](0, elements, afterPath) { path =>
 
    /** This is an absolute path */
    override def absolute = true

    /** Constructs a new path */
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): PathType
    
    /** Returns true if this is a root path */
    def isRoot = elements.isEmpty

    /** Returns the first element of the path, if it exists */
    def head = path.elements.last

    /** Returns the last element of the path, if it exists */
    def last = path.elements.head

    /** Returns all but the first element of the path */
    def tail = drop(1)

    /** Returns all but the last element of the path */
    def init = dropRight(1)

    /** Returns the parent path of this path; equivalent to `tail` */
    def parent: PathType = drop(1)

    /** Constructs a new path by appending the specified path element to this path */
    override def /(p: String): PathType = makePath(0, path.elements ++ Array(p), afterPath)

    override def toString() = pathString
    
    def afterPathString = afterPath.toList sortBy { case (k, (s, p)) => p } map { case (k, (s, p)) => k+s } mkString ""

    def pathString = elements.mkString("/", "/", "")+afterPathString

    /** Drops the specified number of path elements from the left of the path */
    def drop(n: Int): PathType =
      if(path.elements.length - n <= 0) makePath(0, Nil, afterPath)
      else makePath(0, path.elements.dropRight(n), afterPath)
    
    /** Drops the specified number of path elements from the right of the path */
    def dropRight(n: Int): PathType =
      if(path.elements.length - n <= 0) makePath(0, Nil, afterPath)
      else makePath(0, path.elements.drop(n), afterPath)
    
    /** Drops all but the specified number of path elements from the left of the path */
    def take(n: Int): PathType =
      if(path.elements.length - n <= 0) makePath(0, Nil, afterPath)
      else makePath(0, path.elements.take(n), afterPath)
    
    /** Constructs a new path by following the relative link from this path */
    def +[P <: Path[P]](dest: P): Path[_] =
      makePath(0, path.elements.drop(dest.ascent) ++ dest.elements, afterPath)

    def link[P <: AbsolutePath[P]](dest: P)(implicit linkable: Linkable[PathType, P]) = linkable.link(this.asInstanceOf[PathType], dest)

    def -[P <: AbsolutePath[P]](src: P)(implicit linkable: Linkable[P, PathType]) = linkable.link(src, this.asInstanceOf[PathType])

  }

  /** Companion object for simple paths, including a method for creating a path from a `String` */
  object SimplePath {
    def parse(path: String) = new SimplePath(path.replaceAll("^\\/", "").split("/") ++
        (if(path.endsWith("/")) Array("") else Array[String]()), Map())
  }

  /** Defines a very simple absolute path with an unspecified base
    *
    * @param elements The path elements which make up this absolute path */
  class SimplePath(elements: Seq[String], afterPath: AfterPath) extends AbsolutePath[SimplePath](elements, afterPath) {
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath) =
      new SimplePath(elements, afterPath)
  }

  class RelativePath(ascent: Int, elements: Seq[String], afterPath: AfterPath) extends Path[RelativePath](ascent, elements, afterPath) {
    def makePath(a: Int, e: Seq[String], ap: AfterPath): RelativePath = new RelativePath(a, e, ap)
  }

  /** The canonical root for a simple path */
  val ^ = new SimplePath(Nil, Map())

  trait QueryType[-PathType, -Q] {
    def extras(existing: Map[Char, (String, Double)], q: Q): Map[Char, (String, Double)]
  }

  /** Represents a path which is to be considered relative to another (unspecified) path or URL
    *
    * @constructor Creates a new relative path with the specified ascent and elements
    * @param ascent The number of levels to navigate up the path hierarchy to reach the common
    *        parent
    * @param elements The `String` components of this path */
  abstract class Path[+PathType <: Path[PathType]](val ascent: Int, val elements: Seq[String], val afterPath: AfterPath) extends Link { path =>
    
    /** This is a relative path */
    def absolute = false
    
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): PathType
    
    /** Adds a path component to this relative path */
    def /(s: String): PathType = makePath(ascent, Array(s) ++ elements, Map())

    def ?[Q](q: Q)(implicit qt: QueryType[PathType, Q]) = makePath(ascent, elements, qt.extras(afterPath, q))

    override def toString() =
      if(ascent == 0 && elements.isEmpty) "."
      else if(ascent == 0 && elements.head == "" && elements.length == 1) "/"
      else (Array.fill(ascent)("..") ++ elements).mkString("/")
      
    override def equals(that: Any) = that match {
      case p: Path[_] => p.absolute == absolute && p.ascent == ascent && (p.elements.toArray[String]:
          WrappedArray[String]) == (elements.toArray[String]: WrappedArray[String])
      case _ => false
    }
  }

  /** Allows for easy construction of relative paths from `String`s */
  implicit def richString(string: String) = new {
    /** Constructs a relative path from the path components `string` and `string2`
      *
      * @param string2 the second component of the relative path to be constructed */
    def /(string2: String) = new SimplePath(Array(string, string2), Map())
  }

}

