package rapture

trait Finance { this: BaseIo =>

  trait Usd
  trait Gbp
  trait Eur
  trait Chf
  trait Cad
  trait Cny
  trait Dkk
  trait Inr
  trait Jpy
  trait Nok
  trait Nzd
  trait Rub

  class Currency[T](val code: String, val name: String, val dp: Int, val prefix: String)
  implicit object GbpCurrency extends Currency[Gbp]("GBP", "Pounds Sterling", 2, "£")
  implicit object UsdCurrency extends Currency[Usd]("USD", "US Dollars", 2, "$")
  implicit object ChfCurrency extends Currency[Chf]("CHF", "Swiss Francs", 2, "Fr")
  implicit object EurCurrency extends Currency[Eur]("EUR", "Euros", 2, "€")
  implicit object CadCurrency extends Currency[Cad]("CAD", "Canadian Dollars", 2, "$")
  implicit object CnyCurrency extends Currency[Cny]("CNY", "Chinese Yuan", 2, "¥")
  implicit object DkkCurrency extends Currency[Dkk]("DKK", "Danish Krone", 2, "kr")
  implicit object InrCurrency extends Currency[Inr]("INR", "Indian Rupees", 2, "Rs")
  implicit object JpyCurrency extends Currency[Jpy]("JPY", "Japanese Yen", 2, "¥")
  implicit object NokCurrency extends Currency[Nok]("NOK", "Norwegian Krone", 2, "kr")
  implicit object NzdCurrency extends Currency[Nzd]("NZD", "New Zealand Dollars", 2, "$")
  implicit object RubCurrency extends Currency[Rub]("RUB", "Russian Rubles", 2, "р")

  trait MoneyFactory[T] { def apply(d: Double) = new Money[T](d) }

  object Usd extends MoneyFactory[Usd]
  object Gbp extends MoneyFactory[Gbp]
  object Eur extends MoneyFactory[Eur]
  object Chf extends MoneyFactory[Chf]
  trait Cad extends MoneyFactory[Cad]
  trait Cny extends MoneyFactory[Cny]
  trait Dkk extends MoneyFactory[Dkk]
  trait Inr extends MoneyFactory[Inr]
  trait Jpy extends MoneyFactory[Jpy]
  trait Nok extends MoneyFactory[Nok]
  trait Nzd extends MoneyFactory[Nzd]
  trait Rub extends MoneyFactory[Rub]

  case class Money[T: Currency](major: Int, minor: Int) {

    val div = math.pow(10, implicitly[Currency[T]].dp).toInt
    val half = 0.5/div

    def this(amount: Double) =
      this((amount*math.pow(10, implicitly[Currency[T]].dp).toInt + 0.5).toInt/math.pow(10, implicitly[Currency[T]].dp).toInt, (amount*math.pow(10, implicitly[Currency[T]].dp).toInt + 0.5).toInt%math.pow(10, implicitly[Currency[T]].dp).toInt)

    def pad(x: Int) = ("0"*(implicitly[Currency[T]].dp - x.toString.length))+x

    override def toString = implicitly[Currency[T]].prefix+(if(major < 0) "-"+(-major - 1)+"."+pad(div - minor) else major+"."+pad(minor))

    def +(m: Money[T]): Money[T] =
      Money[T](major + m.major + (minor + m.minor)/div, (minor + m.minor)%div)
    
    def unary_- : Money[T] = Money[T](-major - 1, div - minor)
    def -(m: Money[T]): Money[T] = this + -m

    def *(n: Int): Money[T] = this * n.toDouble
    def *(n: Double): Money[T] = {
      val x = ((major*div + minor)*n + 0.5).toInt
      Money[T](x/div, x%div)
    }
  
    def /(n: Int): Money[T] = this * (1.0/n)
    def /(n: Double): Money[T] = this * (1.0/n)
  }

}
