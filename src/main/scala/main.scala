import java.util.{Calendar, Date}


//case class Card(name: String, digits: Long, expires: Date)
//
//case class PayPal(email: String)


enum Payment:
  case Card(name: String, digits: Long, expires: Date)
  case PayPal(email: String)

import Payment.*
def process(kind: Payment) = kind match
  case Card(name, digits, expires) =>
    s"Processing credit card $name, $digits, $expires"
  case PayPal(email) =>
    s"Processing PayPal account $email"


@main
def main(): Unit = {
  val result = process(Card("", 22L, Calendar.getInstance().getTime()))
  println(result)
}