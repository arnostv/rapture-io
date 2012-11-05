/**************************************************************************************************
Rapture I/O Library
Version 0.7.0

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

trait Ips {

  object Ip4 {

    def parse(s: String) = {
      val vs = s.split("\\.").map(_.toInt)
      Ip4(vs(0), vs(1), vs(2), vs(3))
    }
    
    def fromLong(lng: Long) =
      Ip4((lng>>24 & 255L).toInt, (lng>>16 & 255L).toInt, (lng>>8 & 255L).toInt,
          (lng & 255L).toInt)
    
    lazy val privateSubnets = List(Ip4(10, 0, 0, 0)/8, Ip4(192, 168, 0, 0)/16,
        Ip4(172, 16, 0, 0)/12, Ip4(127, 0, 0, 0)/8)
  }

  case class Ip4(b1: Int, b2: Int, b3: Int, b4: Int) {
    
    if(b1 > 255 || b2 > 255 || b3 > 255 || b4 > 255 || b1 < 0 || b2 < 0 || b3 < 0 || b4 < 0)
      throw new InstantiationException("The components of the IP address must be in the range 0-255")

    def asLong = (b1.toLong<<24) + (b2<<16) + (b3<<8) + b4
    def /(i: Int): Subnet = new Subnet(this, i)
    def in(subnet: Subnet) = subnet contains this
    override def toString() = b1+"."+b2+"."+b3+"."+b4
    def isPrivate = Ip4.privateSubnets.exists(in)

    override def equals(that: Any): Boolean = that match {
      case that: Ip4 => b1 == that.b1 && b2 == that.b2 && b3 == that.b3 && b4 == that.b4
      case _ => false
    }

    override def hashCode = b1<<24 | b2<<16 | b3<<8 | b4
  }

  object Subnet {
    def parse(s: String) = {
      val x = s.split("\\/")
      new Subnet(Ip4.parse(x(0)), x(1).toInt)
    }
  }

  class Subnet(baseIp: Ip4, val bits: Int) extends Iterable[Ip4] {

    if(bits < 0 || bits > 32)
      throw new InstantiationException("The subnet size must be in the range 0-32")

    def iterator: Iterator[Ip4] = new Iterator[Ip4] {
      private var current = baseIp.asLong - 1
      def hasNext = current < maximum.asLong
      def next = {
        current += 1
        Ip4.fromLong(current)
      }
    }

    def maximum = Ip4.fromLong((((baseIp.asLong>>(32 - bits)) + 1)<<(32 - bits)) - 1)
    val ip = Ip4.fromLong((baseIp.asLong>>(32 - bits))<<(32 - bits))
    override def size = 1 << (32 - bits)
    override def toString() = ip.toString+"/"+bits
    def contains(ip2: Ip4) = Ip4.fromLong((ip2.asLong>>(32 - bits))<<(32 - bits)) == ip

    override def equals(that: Any) = that match {
      case that: Subnet => ip == that.ip && bits == that.bits
      case _ => false
    }

    override def hashCode = ip.hashCode | bits
  }

  object Localhost extends Ip4(127, 0, 0, 1)
}
