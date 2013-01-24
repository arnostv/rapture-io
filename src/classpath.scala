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

trait Classpath { this: BaseIo =>

  class ClasspathUrl(elements: Seq[String]) extends Url[ClasspathUrl](elements, Map()) {
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath) =
      new ClasspathUrl(elements)
    
    def schemeSpecificPart = elements.mkString("//", "/", "")
    val pathRoot = Classpath
  }

  object Classpath extends PathRoot[ClasspathUrl] with Scheme[ClasspathUrl] {
    def schemeName = "classpath"
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath) =
      new ClasspathUrl(elements)
    def scheme = Classpath
  }

  implicit object ClasspathStreamByteReader extends StreamReader[ClasspathUrl, Byte] {
    def input(url: ClasspathUrl): ![Exception, Input[Byte]] =
      except(new ByteInput(this.getClass.getClassLoader.getResourceAsStream(
          url.pathString.substring(1))))
  }

  implicit def classpathStreamCharReader(implicit enc: Encoding) =
    new StreamReader[ClasspathUrl, Char] {
      def input(url: ClasspathUrl): ![Exception, Input[Char]] =
        except(inputStreamCharBuilder(enc).input(this.getClass.getClassLoader.getResourceAsStream(
            url.pathString.substring(1))))
    }

}
