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

trait Linking { this: Io =>

  /** Calculates the relative link between this path and the specified destination path
    *
    * FIXME: This probably scales badly as path lengths increase.
    *
    * @param dest The destination path to calculate the relative link to
    * @return The calculated relative path */
  def generalLink(src: List[String], dest: List[String]): (Int, List[String]) =
    yCombinator[(Int, List[String], List[String], List[String]), (Int, List[String])] { fn => v =>
      v match {
        case (0, Nil, x :: x2 :: xs, y :: ys) if x == y =>
          fn((0, Nil, x2 :: xs, ys))
        
        case (up, tail, x :: Nil, y :: Nil) if x == y =>
          (0, List[String]())
        
        case (up, tail, x :: Nil, y :: ys) =>
          (up, tail.reverse ::: List(y) ::: ys)
        
        case (0, _, Nil, ys) =>
          (1, List[String](dest.head))
        
        case (up, tail, Nil, ys) =>
          (up, tail.reverse ::: ys)
        
        case (up, tail, _ :: x :: xs, Nil) =>
          fn((up + 1, tail, x :: xs, Nil))

        case (up, tail, x :: Nil, Nil) =>
          fn((up, tail, Nil, Nil))

        case (up, tail, _ :: x :: xs, y :: ys) =>
          fn((up + 1, y :: tail, x :: xs, ys))

      }
    } (0, Nil, if(src == Nil) List("") else src, if(dest == Nil) List("") else dest)

  trait Linkable[-Src, -Dest] {
    type Result <: Link
    def link(src: Src, dest: Dest): Result
  }

  implicit object SimplePathsLinkable extends Linkable[SimplePath, SimplePath] {
    type Result = RelativePath
    def link(src: SimplePath, dest: SimplePath): RelativePath = {
      val lnk = generalLink(src.elements.to[List], dest.elements.to[List])
      new RelativePath(lnk._1, lnk._2, dest.afterPath)
    }
  }

  implicit object HttpUrlLinkable extends Linkable[HttpUrl, HttpUrl] {
    type Result = Link
    def link(src: HttpUrl, dest: HttpUrl) = {
      if(src.ssl == dest.ssl && src.hostname == dest.hostname && src.port == dest.port) {
        val lnk = generalLink(src.elements.to[List], dest.elements.to[List])
        new RelativePath(lnk._1, lnk._2, dest.afterPath)
      } else dest
    }
  }

  implicit object HttpFileUrlLinkable extends Linkable[FileUrl, HttpUrl] {
    type Result = HttpUrl
    def link(src: FileUrl, dest: HttpUrl) = dest
  }

  implicit object HttpFileUrlsLinkable extends Linkable[HttpUrl, FileUrl] {
    type Result = FileUrl
    def link(src: HttpUrl, dest: FileUrl) = dest
  }
}
