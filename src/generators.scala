package rapture.io

object Generator {

  def nonNull[T](f : => T) = non[T](null.asInstanceOf[T])(f)

  def non[T](halt : T)(f : => T) = new Iterator[T] {
    private var n : T = _
    private var h = false
    
    def hasNext = {
      if(!h) {
        n = f
        h = true
      }
      n != halt
    }

    def next = {
      if(!h) n = f
      h = false
      n
    }
  }

}
