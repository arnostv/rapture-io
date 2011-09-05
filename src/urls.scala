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

import java.io._
import java.net._

/** Provides framework classes and traits for handling general URLs. */
trait Urls { this : Io =>

  /** Represents a generic URL */
  trait Url[+UrlType <: Url[UrlType]] extends AbsolutePath[UrlType] { thisPath : UrlType =>
    
    /** The base for this URL */
    val urlBase : UrlBase[UrlType]

    /** Delegate to make the URL's scheme from the UrlBase visible from the URL */
    def scheme = urlBase.scheme

    override def toString() = urlBase.toString()+super.toString()
    
    /** Constructs a new URL by appending the given path element to the path. */
    override def /(element : String) = urlBase.makePath(thisPath.elements ++ Array(element))
    
    /** Constructs a new URL by calculating the destination URL by following  the given
      * `RelativePath` from this URL */
    def /(path : RelativePath) = urlBase.makePath(path.elements)
    
    /** Calculates the destination of the given link from this URL
      *
      * @param path the relative path */
    override def ++(dest : RelativePath) : UrlType =
      urlBase.makePath(dest.elements ++ thisPath.elements.drop(dest.ascent))

    /** Calculates the path between this URL and the given destination URL, if possible as a
      * relative path when the source and destination paths have the same base, or - failing that -
      * simply returns the destination path.
      *
      * @param dest The destination path to which the link should be calculated
      * @tparam PathType The type of the path being linked to
      * @return The relative link between this `Url` and the destination `Url`, if possible, or the
      *         destination path if not.
      * */
    override def link[PathType <: AbsolutePath[PathType]](dest : AbsolutePath[PathType]) : Path =
      if(dest.isInstanceOf[Url[_]] && dest.asInstanceOf[Url[_]].urlBase == urlBase) super.link(dest)
      else dest
  }

  /** Repenesents a URL scheme */
  case class Scheme[+U <: Url[U]](schemeName : String)

  /** Defines a base to upon which the hierarchical part of the URL is appended */
  trait UrlBase[+U <: Url[U]] {
    override def toString() = scheme.schemeName+"://"
    def scheme : Scheme[U]
    def makePath(elements : Seq[String]) : U
  }

  /** Specifies additional methods for URLs which have a hierarchical structure. */
  trait PathUrl[+UrlType <: Url[UrlType]] { pathUrl : UrlType =>
    def /(d : String) : UrlType
    def /(path : RelativePath) : UrlType
  }
}
