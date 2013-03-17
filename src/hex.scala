package rapture

trait Hex { this: BaseIo =>

  object Hex {
    def encode(a: Array[Byte]): String =
      new String(a flatMap { n => Array((n & 255) >> 4 & 15, n & 15) } map { _ + 48 } map { i =>
        (if(i > 57) i + 39 else i).toChar })

    def decode(s: String): ![Exception, Array[Byte]] = except {
      (if(s.length%2 == 0) s else "0"+s).to[Array].grouped(2).to[Array] map { case Array(l, r) =>
        (((l - 48)%39 << 4) + (r - 48)%39).toByte
      }
    }
  }

}
