package tests.rapture.io

import rapture.io._

object HttpSample {
  def main(args: Array[String]): Unit = {
    println("posting say->Hello world to http://rapture.io/repeat")

    val url = Http / "rapture.io" / "repeat"

    val response: HttpResponse = url post Map(
      'say -> "Hello world"
    )

    val responseString = response.slurp[Char]

    println(s"Status ${response.status}")
    println(responseString)
  }
}
