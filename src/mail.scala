package rapture.io

trait Mail { this: Io =>

  object Mailto extends Scheme[MailtoUri] {
    def schemeName = "mailto"
    def /(email: String): MailtoUri = new MailtoUri(email)
  }

  class MailtoUri(email: String) extends Uri {
    def scheme = Mailto
    def schemeName = scheme.schemeName
    def schemeSpecificPart = email
  }

}
