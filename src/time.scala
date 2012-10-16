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

package rapture.io

object Time {

  val shortUsDateFormat = DateFormat("MM/dd/yy")
  val shortEuropeDateFormat = DateFormat("dd/MM/yy")
  val longUsDateFormat =  DateFormat("MMMM d, yyyy")
  val longEuropeDateFormat = DateFormat("d MMMM yyyy")

  val HmsTimeFormat = TimeFormat("HH:mm:ss")
  val HmTimeFormat = TimeFormat("HH:mm")
  val alternativeTimeFormat = TimeFormat("H.mma")

  implicit def intExtras(i: Int) = new {
    def seconds = i*1000
    def minutes = seconds*60
    def hours = minutes*60
    def days = hours*24
    def weeks = days*7
    def months = days*30
    def years = days*365
  }


  case class DateFormat(pattern: String) {
    def format(d: Date): String =
      new java.text.SimpleDateFormat(pattern).format(new java.util.Date(d.toLong))
    
    def format(dt: DateTime): String =
      new java.text.SimpleDateFormat(pattern).format(new java.util.Date(dt.toLong))
  }

  case class TimeFormat(pattern: String) {
    def format(dt: DateTime): String =
      new java.text.SimpleDateFormat(pattern).format(new java.util.Date(dt.toLong))
  }

  implicit val dateOrder = new Ordering[Date] {
    def compare(d1: Date, d2: Date) = if(d1 < d2) -1 else if(d2 == d1) 0 else 1
  }

  implicit val dateTimeOrder = new Ordering[DateTime] {
    def compare(d1: DateTime, d2: DateTime) = if(d1 < d2) -1 else if(d2 == d1) 0 else 1
  }

  def now() = DateTime.unapply(System.currentTimeMillis).get

  object Date {
    def unapply(n: Long) = {
      val d = new java.util.Date(n)
      Some(Date(d.getYear + 1900, d.getMonth + 1, d.getDate))
    }
  }

  object DateTime {
    def unapply(n: Long) = {
      val Date(date) = n
      val d = new java.util.Date(n)
      Some(DateTime(date, d.getHours, d.getMinutes, d.getSeconds))
    }
  }

  case class Date(year: Int, month: Int, day: Int) { date =>
  
    override def toString() =
      day+"-"+monthString(month)+"-"+year

    def +(n: Int) = Date.unapply(n + toLong).get
    def -(n: Int) = Date.unapply(toLong - n).get

    override def equals(that: Any): Boolean = that match {
      case that: Date => toLong == that.toLong
      case that: DateTime => toLong == that.toLong
      case _ => false
    }

    def toLong = {
      val d = new java.util.Date(0L)
      d.setYear(year - 1900)
      d.setMonth(month - 1)
      d.setDate(day)
      d.getTime
    }
    
    def at(hours: Int) = new {
      def h(minutes: Int) = new DateTime(date, hours, minutes, 0) {
        def m(seconds: Int) = DateTime(date, hours, minutes, seconds)
      }
    }
    
    def >(that: Date): Boolean = toLong > that.toLong
    def <(that: Date): Boolean = toLong < that.toLong
    def >=(that: Date): Boolean = toLong >= that.toLong
    def <=(that: Date): Boolean = toLong <= that.toLong

    def format(implicit dateFormat: DateFormat) = dateFormat.format(this)
  }

  case class DateTime(date: Date, hour: Int, minute: Int, second: Int) {
    
    def pad(n: Int) = if(n < 10) "0"+n else n

    def +(n: Int) = DateTime.unapply(n + toLong).get
    def -(n: Int) = DateTime.unapply(toLong - n).get

    override def toString() =
      date.toString+" "+pad(hour)+":"+pad(minute)+":"+pad(second)

    override def equals(that: Any): Boolean = that match {
      case that: Date => toLong == that.toLong
      case that: DateTime => toLong == that.toLong
      case _ => false
    }

    def >(that: DateTime): Boolean = toLong > that.toLong
    def <(that: DateTime): Boolean = toLong < that.toLong
    def >=(that: DateTime): Boolean = toLong >= that.toLong
    def <=(that: DateTime): Boolean = toLong <= that.toLong

    def toLong = {
      val d = new java.util.Date(0L)
      d.setYear(date.year - 1900)
      d.setMonth(date.month - 1)
      d.setDate(date.day)
      d.setHours(hour)
      d.setMinutes(minute)
      d.setSeconds(second)
      d.getTime
    }
    
    def format(implicit dateFormat: DateFormat, timeFormat: TimeFormat) =
      dateFormat.format(this)+" "+timeFormat.format(this)
  }

  case class Month(no: Int)

  def monthString(n: Int) = List("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
      "Oct", "Nov", "Dec")(n - 1)

  val Jan = Month(1)
  val Feb = Month(2)
  val Mar = Month(3)
  val Apr = Month(4)
  val May = Month(5)
  val Jun = Month(6)
  val Jul = Month(7)
  val Aug = Month(8)
  val Sep = Month(9)
  val Oct = Month(10)
  val Nov = Month(11)
  val Dec = Month(12)

  implicit def intoMonth(d: Int) = new {
    def -(m: Month) = new {
      def -(y: Int) = Date(y, m.no, d)
    }
  }

}
