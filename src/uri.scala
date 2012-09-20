package rapture.io

trait Uris { this: Io =>

  trait Link

  trait Uri extends Link {
    def scheme: Scheme[_]
    def schemeSpecificPart: String
    override def toString() = scheme.schemeName+":"+schemeSpecificPart
  }

}
