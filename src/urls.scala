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

import java.io._
import java.net._

/** Provides framework classes and traits for handling general URLs. */
trait Urls { this: Io =>

  /** Represents a generic URL */
  abstract class Url[+UrlType <: Url[UrlType]](elements: Seq[String], afterPath: String) extends
      AbsolutePath[UrlType](elements, afterPath) with Uri { thisPath: UrlType =>
  
    /** The base for this URL */
    val pathRoot: PathRoot[UrlType]

    /** Delegate to make the URL's scheme from the PathRoot visible from the URL */
    def scheme = pathRoot.scheme

    /** The string version of the scheme, for the benefit of the Uri trait */
    def schemeName = scheme.toString

    /** Constructs a new URL by appending the given path element to the path. */
    override def /(element: String) = pathRoot.makePath(0, thisPath.elements ++ Array(element), afterPath)
    
    /** Constructs a new URL by calculating the destination URL by following the given
      * `Path` from this URL */
    def /(path: SimplePath) = pathRoot.makePath(0, path.elements, afterPath)
    
    /** Calculates the destination of the given link from this URL
      *
      * @param path the relative path */
    override def +[P <: Path[P]](dest: P): Path[_] =
      if(dest.absolute) dest
      else pathRoot.makePath(0, dest.elements ++ thisPath.elements.drop(dest.ascent), afterPath)
  }

  /** Repenesents a URL scheme */
  trait Scheme[+U <: Uri] {
    def schemeName: String
    override def toString() = schemeName
  }

  /** Defines a base to upon which the hierarchical part of the URL is appended */
  abstract class PathRoot[+U <: Url[U]] extends AbsolutePath[U](Nil, "") {
    def scheme: Scheme[U]
    def makePath(ascent: Int, elements: Seq[String], afterPath: String): U
  }

  /** Specifies additional methods for URLs which have a hierarchical structure. */
  trait PathUrl[+UrlType <: Url[UrlType]] { pathUrl: UrlType =>
    def /(d: String): UrlType
    def /(path: SimplePath): UrlType
  }
}
