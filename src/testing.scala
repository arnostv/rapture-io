/**************************************************************************************************
Rapture Test Library
Version 0.5.0

The primary distribution site is

  http://rapture.io/

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

trait AnsiCodes {

  object Ansi {
    val Esc = 27.toChar
    val Normal = Esc+"[0m"
    val Bold = Esc+"[1m"
    val Underline = Esc+"[4m"
    val Blink = Esc+"[5m"
    val Reverse = Esc+"[7m"
    val Nondisplayed = Esc+"[8m"
    val Black = Esc+"[30m"
    val Red = Esc+"[31m"
    val Green = Esc+"[32m"
    val Yellow = Esc+"[33m"
    val Blue = Esc+"[34m"
    val Magenta = Esc+"[35m"
    val Cyan = Esc+"[36m"
    val White = Esc+"[37m"
    val BgBlack = Esc+"[30m"
    val BgRed = Esc+"[31m"
    val BgGreen = Esc+"[32m"
    val BgYellow = Esc+"[33m"
    val BgBlue = Esc+"[34m"
    val BgMagenta = Esc+"[35m"
    val BgCyan = Esc+"[36m"
    val BgWhite = Esc+"[37m"
    def cursor(row : Int, col : Int) = Esc+"["+row+";"+col+"H"
    def up(n : Int) = Esc+"["+n+"A"
    def down(n : Int) = Esc+"["+n+"B"
    def right(n : Int) = Esc+"["+n+"C"
    def left(n : Int) = Esc+"["+n+"D"
  }
}
