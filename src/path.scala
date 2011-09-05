/**************************************************************************************************
Rapture I/O Library
Version 0.6.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2011 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/

package rapture.io

import scala.collection.mutable.WrappedArray

import java.io._
import java.net._

/** Defines generalised classes for handling hierarchical path structures and linking between them.
  * This forms the basis for hierarchical URLs. */
trait Paths { this : Io =>

  val $ = ""

  trait Path {

    val elements : Seq[String]
    
    override def equals(that : Any) : Boolean =
      that.isInstanceOf[Path] && (that.asInstanceOf[Path].elements.toArray[String] :
          WrappedArray[String]) == (elements.toArray[String] : WrappedArray[String])
      // ^^^ FIXME
      // that.isInstanceOf[Path] && that.asInstanceOf[Path].elements == elements
  }

  object / {
    def unapply(p : SimplePath) : Option[(SimplePath, String)] =
      if(p.isRoot) None else Some((p.init, p.last))
  }

  /** Represents an absolute (i.e. relative to a canonical base) path. */
  trait AbsolutePath[+PathType <: AbsolutePath[PathType]] extends Path { path =>
  
    /** The components of the path */
    val elements : Seq[String]

    /** Constructs a new path */
    def makePath(elements : Seq[String]) : PathType
    
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
    def parent : PathType = drop(1)

    /** Constructs a new path by appending the specified path element to this path */
    def /(p : String) : AbsolutePath[PathType] = makePath(path.elements ++ Array(p))

    override def toString() = pathString
    
    def pathString = elements.mkString("/", "/", "")

    /** Drops the specified number of path elements from the left of the path */
    def drop(n : Int) : PathType =
      if(path.elements.length - n <= 0) makePath(Nil)
      else makePath(path.elements.dropRight(n))
    
    /** Drops the specified number of path elements from the right of the path */
    def dropRight(n : Int) : PathType =
      if(path.elements.length - n <= 0) makePath(Nil)
      else makePath(path.elements.drop(n))
    
    /** Drops all but the specified number of path elements from the left of the path */
    def take(n : Int) : PathType =
      if(path.elements.length - n <= 0) makePath(Nil)
      else makePath(path.elements.takeRight(n))
    
    /** Constructs a new path by following the relative link from this path */
    def ++(dest : RelativePath) = makePath(path.elements.drop(dest.ascent) ++ dest.elements)

    /** Calculates the relative link between this path and the specified destination path
      *
      * FIXME: ^ / "a" / "b" link ^ / "a" does not work
      *
      * @param dest The destination path to calculate the relative link to
      * @return The calculated relative path */
    def link[DestPathType <: AbsolutePath[DestPathType]](dest : AbsolutePath[DestPathType]) :
        Path = {
      
      def go(from : List[String], to : List[String], up : Int, tail : List[String])
          : Path = (up, tail, from, to) match {
        case (0, Nil, x :: x2 :: xs, y :: ys) if x == y =>
          println("A: "+(up, tail, from, to)+", "+up)
          go(x2 :: xs, ys, up, tail)
        
        case (_, _, x :: Nil, y :: Nil) if x == y =>
          println("B: "+(up, tail, from, to)+", "+up)
          new RelativePath(0, Array[String]())
        
        case (_, _, x :: Nil, y :: ys) =>
          println("C: "+(up, tail, from, to)+", "+up)
          new RelativePath(up, tail.reverse.toArray[String] ++ Array(y) ++ ys)
        
        case (0, _, Nil, ys) =>
          new RelativePath(1, Array[String](dest.head))
        
        case (_, _, Nil, ys) =>
          new RelativePath(up, tail.reverse.toArray[String] ++ ys.toArray[String])
        
        case (_, _, _ :: x :: xs, Nil) =>
          println("E: "+(up, tail, from, to)+", "+up)
          go(x :: xs, Nil, up + 1, tail)
        
        case (_, _, x :: Nil, Nil) =>
          println("F: "+(up, tail, from, to)+", "+up)
          go(Nil, Nil, up, tail)
        
        case (_, _, _ :: x :: xs, y :: ys) =>
          println("G: "+(up, tail, from, to)+", "+up)
          go(x :: xs, ys, up + 1, y :: tail)
      }

      go(if(isRoot) List("") else path.elements.toList, if(dest.isRoot) List("") else
          dest.elements.toList, 0, Nil)
    }
  }

  /** Companion object for simple paths, including a method for creating a path from a `String` */
  object SimplePath {
    def fromString(path : String) = new SimplePath(path.replaceAll("^\\/", "").split("/") ++
        (if(path.endsWith("/")) Array("") else Array[String]()))
  }

  /** Defines a very simple absolute path with an unspecified base
    *
    * @param elements The path elements which make up this absolute path */
  class SimplePath(val elements : Seq[String]) extends AbsolutePath[SimplePath] {
    def makePath(elements : Seq[String]) = new SimplePath(elements)
  }

  /** The canonical root for a simple path */
  object ^ extends SimplePath(Nil)

  /** Represents a path which is to be considered relative to another path or URL
    *
    * @constructor Creates a new relative path with the specified ascent and elements
    * @param ascent The number of levels to navigate up the path hierarchy to reach the common
    *        parent
    * @param elements The `String` components of this path */
  class RelativePath(val ascent : Int, val elements : Seq[String]) extends Path { path =>
    
    /** Adds a path component to this relative path */
    def /(s : String) : RelativePath = new RelativePath(ascent, Array(s) ++ elements)

    override def toString() =
      if(ascent == 0 && elements.isEmpty) "."
      else if(ascent == 0 && elements == Array("")) "/"
      else (Array.fill(ascent)("..") ++ elements).mkString("/")
  }

  /** Allows for easy construction of relative paths from `String`s */
  implicit def richString(string : String) = new {
    /** Constructs a relative path from the path components `string` and `string2`
      *
      * @param string2 the second component of the relative path to be constructed */
    def /(string2 : String) = new RelativePath(0, Array(string, string2))
  }

}

