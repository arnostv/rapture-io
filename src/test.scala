package rapture.io

object Test {

  def time[T](blk: => T) = {
    val t0 = System.currentTimeMillis
    blk -> (System.currentTimeMillis - t0)
  }

/*  def apply() = {

    println("TEST 1")
    var t = 0L
    for(i <- 1 to 10) {
      val tn = (time { (File / "run" / "test1") > (File / "run" / "test2") })._2
      println("Time "+i+": "+tn+"ms")
      t += tn
    }
    println("Average: "+(t/10)+"ms")

    println()
    println("TEST 2")
    for(i <- 1 to 10) {
      import java.io._
      val is = new FileInputStream(new File("/run/test1"))
      val os = new FileOutputStream(new File("/run/test2"))
      val buf = new Array[Byte](65536)
      var len = read

    }

  }
*/
}
