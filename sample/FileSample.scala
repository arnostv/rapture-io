import rapture.io._

object FileSample {

  def main(args: Array[String]) {
    val file  = File.currentDir / "sample" / "sample1.txt"
    println(s"Reading file ${file.pathString}")

    println(file.slurp[Char])
  }

}
