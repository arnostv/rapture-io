package rapture.io

import scala.collection._
import scala.collection.generic._

trait CollectionExtras {

  @inline implicit class SeqExtras[A, C[A] <: Seq[A]](val xs: C[A]) {
    
    def intersperse[B >: A, That](between: B)(implicit bf: CanBuildFrom[C[A], B, That]): That = {
      val b = bf(xs)
      xs.init foreach { x =>
        b += x
        b += between
      }
      b += xs.last
      b.result
    }
    
    def intersperse[B >: A, That](before: B, between: B, after: B)(implicit bf: CanBuildFrom[C[A], B, That]): That = {
      val b = bf(xs)
      b += before
      xs.init foreach { x =>
        b += x
        b += between
      }
      b += xs.last
      b += after
      b.result
    }

    def zipWith[T](fn: A => T)(implicit bf: CanBuildFrom[C[A], (A, T), C[(A, T)]]): C[(A, T)] = {
      val b = bf(xs)
      xs.foreach { x => b += (x -> fn(x)) }
      b.result
    }
  }
}
