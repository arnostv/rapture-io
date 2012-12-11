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

import language.implicitConversions

import java.io._
import java.net._

/** Provides support for accessing the file system through FileUrls. This is a wrapper for Java's
  * file handling facilities, and provides roughly the same functionality within the general URL
  * framework. */
trait Files { this: Io =>

  /** Type class object for writing `FileUrl`s as `Output[Stream]`s */
  implicit object FileStreamCharWriter extends StreamWriter[FileUrl, Char] {
    def output(url: FileUrl): ![Output[Char]] =
      except(new CharOutput(new BufferedWriter(new FileWriter(url.javaFile))))
  }

  implicit object FileStreamCharAppender extends StreamAppender[FileUrl, Char] {
    def appendOutput(url: FileUrl): ![Output[Char]] =
      except(new CharOutput(new BufferedWriter(new FileWriter(url.javaFile, true))))
  }

  /** Type class object for reading `FileUrl`s as `Input[Stream]`s */
  implicit object FileStreamCharReader extends StreamReader[FileUrl, Char] {
    def input(url: FileUrl): ![Input[Char]] =
      except(new CharInput(new BufferedReader(new FileReader(new java.io.File(url.pathString)))))
  }

  /** The file scheme object used as a factory for FileUrls. */
  object File extends PathRoot[FileUrl] with Scheme[FileUrl] { thisPathRoot =>

    def schemeName = "file"

    /** Provides a FileUrl for the current working directory, as determined by the user.dir
      * environment variable. */
    def currentDir = makePath(0, System.getProperty("user.dir").split("/").filter(_ != ""), Map())
    
    /** Method for creating a new instance of this type of URL.
      *
      * @param elements The elements of the path for the new FileUrl to create */
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): FileUrl =
      new FileUrl(thisPathRoot, elements.toArray[String])
    
    /** Method for creating a new FileUrl from a java.io.File. */
    def apply(file: java.io.File) = makePath(0, file.getAbsolutePath.split("\\/"), Map())
    
    /** Reference to the scheme for this type of URL */
    def scheme: Scheme[FileUrl] = File
   
    /** Creates a new FileUrl of the specified resource in the filesystem root.
      *
      * @param resource the resource beneath the filesystem root to create. */
    override def /(resource: String) = makePath(0, Array(resource), Map())
  }

  trait Navigable[UrlType] {
    def children(url: UrlType): ![List[UrlType]]
    
    /** Returns false if the filesystem object represented by this FileUrl is a file, and true if
      * it is a directory. */
    def isDirectory(url: UrlType): ![Boolean]
    
    /** If this represents a directory, returns an iterator over all its descendants,
      * otherwise returns the empty iterator. */
    def descendants(url: UrlType): ![Iterator[UrlType]] = except(children(url).iterator.flatMap { c =>
      if(isDirectory(c)) Iterator(c) ++ descendants(c) else Iterator(c)
    })
  }

  implicit def navigableExtras[UrlType: Navigable](url: UrlType) = new {
    
    /** Return a list of children of this URL */
    def children = implicitly[Navigable[UrlType]].children(url)
    
    /** Return true if this URL node is a directory (i.e. it can contain other URLs). */
    def isDirectory: ![Boolean] = implicitly[Navigable[UrlType]].isDirectory(url)

    /** Return an iterator of all descendants of this URL. */
    def descendants: ![Iterator[UrlType]] = implicitly[Navigable[UrlType]].descendants(url)
  }

  /** Specifies how file: URLs should be navigable. */
  implicit val NavigableFile = new Navigable[FileUrl] {
    def children(url: FileUrl): ![List[FileUrl]] = 
      except(if(url.isFile) Nil else (url.javaFile.list().to[List]) map { fn: String => url./(fn) })
    
    def isDirectory(url: FileUrl) = except(url.javaFile.isDirectory())
  }

  /** Defines a URL for the file: scheme, and provides standard filesystem operations on the file
    * represented by the URL. */
  class FileUrl(val pathRoot: PathRoot[FileUrl], elements: Seq[String]) extends Url[FileUrl](elements, Map())
      with PathUrl[FileUrl] {

    /** The java.io.File corresponding to this FileUrl. */
    lazy val javaFile: java.io.File = new java.io.File(pathString)
    
    /** The scheme-specific part of the URL, which appears after the colon */
    def schemeSpecificPart = "//"+pathString
    
    /** Returns true if the file or directory represented by this FileUrl can be read from. */
    def readable: Boolean = javaFile.canRead()
   
    /** Returns true if the file or directory represented by this FileUrl can be written to. */
    def writable: Boolean = javaFile.canWrite()
    
    /** Deletes the file represented by this FileUrl. If the recursive flag is set and the
      * filesystem object is a directory, all subfolders and their contents will also be
      * deleted. */
    def delete(recursive: Boolean = false): ![Boolean] =
      except(if(recursive) deleteRecursively() else javaFile.delete())
    
    /** Add a hook to the filesystem to delete this file upon shutdown of the JVM. */
    def deleteOnExit(): Unit = javaFile.deleteOnExit()
    
    /** Returns true if this object exists on the filesystem. */
    def exists: Boolean = javaFile.exists()
    
    /** Returns the filename of this filesystem object. */
    def filename: String = javaFile.getName()
    
    /** Returns true if the filesystem object represented by this FileUrl is a file, and false if
      * it is a directory. */
    def isFile: Boolean = javaFile.isFile()
    
    /** Returns true if the file or directory is hidden. */
    def hidden: Boolean = javaFile.isHidden()
   
    /** Returns the date of the last modification to the file or directory. */
    def lastModified: ![java.util.Date] = except(javaFile.lastModified() match {
      case 0L => throw new IOException
      case d => new java.util.Date(d)
    })
    
    /** Returns the size of the file in bytes. */
    def length: ![Long] = except(javaFile.length() match {
      case 0L if !exists => throw new IOException
      case x => x
    })
    
    /** Returns the size of the file in bytes. */
    def size: ![Long] = length
    
    /** Creates a new instance of this type of URL. */
    def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): FileUrl =
      File.makePath(ascent, elements, afterPath)
    
    /** If the filesystem object represented by this FileUrl does not exist, it is created as a
      * directory, provided that either the immediate parent directory already exists, or the
      * makeParents path is set. */
    def mkdir(makeParents: Boolean = false): ![Boolean] =
      except(if(makeParents) javaFile.mkdirs() else javaFile.mkdir())
    
    /** Renames this file to a new location. */
    def renameTo(dest: FileUrl): Boolean = javaFile.renameTo(dest.javaFile)
    
    /** Copies this file to a new location specified by the dest parameter. */
    def copyTo(dest: FileUrl)(implicit sr: StreamReader[FileUrl, Byte]): Boolean =
      sr.pump(this, dest) > 0
    
    /** Moves this file to a new location specified by the dest parameter. This will first attempt
      * to move the file by renaming it, but will attempt copying and deletion if renaming fails. */
    def moveTo(dest: FileUrl): ![Boolean] = except(renameTo(dest) || copyTo(dest) && delete())

    /** Update the last-modified time of this file to the current time. */
    def touch() = lastModified = new java.util.Date

    /** Set the last modified time of this file or directory. */
    def lastModified_=(d: java.util.Date) = javaFile.setLastModified(d.getTime)
    
    /** Extract the file extension from the name of this file. */
    def extension: ![Option[String]] =
      except(if(filename contains ".") Some(filename.split("\\.").last) else None)
    
    /** Attempt to alter the permissions of this file so that it is writable. */
    def writable_=(b: Boolean) =
      if(!b) javaFile.setReadOnly() else writable || (throw new IOException("Can't set writable"))
    
    /** Creates a temporary file beneath this directory with the prefix and suffix specified. */
    def tempFile(prefix: String = "tmp", suffix: String = ""): ![FileUrl] =
      except(File(java.io.File.createTempFile(prefix, suffix, javaFile)))
    
    private def deleteRecursively(): Boolean = {
      if(NavigableFile.isDirectory(this)) NavigableFile.children(this).foreach(_.deleteRecursively())
      delete()
    }
  }
}

