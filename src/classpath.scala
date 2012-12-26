package rapture

trait Classpath { this: Io =>

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
