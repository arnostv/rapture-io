package rapture.io

object IpAddress {

  def parse(s : String) = {
    val vs = s.split("\\.").map(_.toInt)
    IpAddress(vs(0), vs(1), vs(2), vs(3))
  }
  
  def fromLong(lng : Long) =
    IpAddress((lng>>24 & 255L).toInt, (lng>>16 & 255L).toInt, (lng>>8 & 255L).toInt,
        (lng & 255L).toInt)
  
  lazy val privateSubnets = List(IpAddress(10, 0, 0, 0), IpAddress(192, 168, 0, 0),
      IpAddress(172, 16, 0, 0))
}

case class IpAddress(b1 : Int, b2 : Int, b3 : Int, b4 : Int) {
  def asLong = (b1.toLong<<24) + (b2<<16) + (b3<<8) + b4
  def /(i : Int) = IpAddress.fromLong((asLong>>(32 - i))<<(32 - i))
  def inSubnet(ip : IpAddress) = (asLong & ip.asLong) == ip.asLong
  override def toString() = b1+"."+b2+"."+b3+"."+b4
  def isPrivate = IpAddress.privateSubnets.exists(inSubnet)
}

object Localhost extends IpAddress(127, 0, 0, 1)
