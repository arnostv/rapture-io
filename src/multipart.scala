package rapture.io

import scala.collection.mutable.{HashMap, ListBuffer}

trait Multipart {

  case class Multipart(data: Array[Byte], headers: Map[String, String]) {
    def contentType: Option[MimeTypes.MimeType] =
      headers.get("Content-Type") map { t =>
        MimeTypes.fromString(t).getOrElse(MimeTypes.MimeType(t))
      }

    lazy val contentDisposition: Option[(String, Map[String, String])] =
      headers.get("Content-Disposition") flatMap { v =>
        v.split("; *").toList match {
          case h :: t =>
            val m = t map { a => val b = a.split("="); b(0) -> b(1) } toMap

            Some(h -> m)
          case _ => None
        }
      }

    def filename: Option[String] = contentDisposition flatMap { case (_, m) => m.get("filename") }

    def name: Option[String] = contentDisposition flatMap { case (_, m) => m.get("name") }

    def disposition: Option[String] = contentDisposition.map(_._1)

  }

  class MultipartReader(boundary: String, in: java.io.InputStream) extends Input[Multipart] {
    
    implicit val logZone = Zone("multipart")
    
    private val bnd = ("--"+boundary).getBytes("ASCII")
    
    private var finished = false
    private var cued = next()

    def close(): Unit = in.close()
    def ready(): Boolean = cued.isDefined
    def read(): Option[Multipart] = {
      val r = cued
      cued = next()
      r
    }

    private def next(): Option[Multipart] = {
      
      var buf: Array[Byte] = null
      var count = -1
      val bufs = new ListBuffer[Array[Byte]]()

      var headers: Map[String, String] = Map()
      var dataStart = 0
      var boundmatch: Int = 0

      while(!finished) {
        
        var cur = in.read()
        
        if(buf != null && dataStart == 0 && (buf(count%65536) == 10 && cur == 13 || buf(count%65536) == 13 && cur == 10)) {
        } else if(buf != null && dataStart == 0 && (cur == 10 || cur == 13 && buf(count%65536) == cur)) {
          dataStart = count + 1
          val next = in.read().toByte
          if(next != 10 && next != 13) {
            count += 1
            buf(count%65536) = next
          }
          headers = new String(buf.slice(1, dataStart), "ISO-8859-1").split("\r") map { h =>
            val i = h.indexOf(':')
            h.substring(0, i) -> h.substring(i + 2, h.length)
          } toMap
        } else {
          count += 1
          
          if(cur == -1) {
            finished = true
            return None
          }
          
          if(count%65536 == 0) {
            buf = new Array[Byte](65536)
            bufs += buf
          }
          
          buf(count%65536) = cur.toByte
          
          boundmatch = if(buf(count%65536) == bnd(boundmatch)) boundmatch + 1 else 0
          
          if(boundmatch == bnd.length) {
            val dataEnd = count - boundmatch - 1
            val size = dataEnd - dataStart
            
            if(size > 0) {
              val res = new Array[Byte](size)
              var done = 0
              var i = 0
              var offset = dataStart
              while(done < size) {
                val chunk = List(65536 - offset, size - done).min
                System.arraycopy(bufs(i), offset, res, done, chunk)
                done += chunk
                offset = 0
                i += 1
              }
              
              return Some(Multipart(res, headers))
            } else {
              count = -1
              bufs.clear()
              buf = null
              boundmatch = 0
              dataStart = 0
            }
          }
        }
      }
      None
    }
  }
}
