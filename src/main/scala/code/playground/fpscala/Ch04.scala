package code.playground.fpscala.ch04


enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None => None

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(a) => f(a)
    case None => None

  def getOrElse[B >: A](default: => B): B = this match
    case Some(a) => a
    case None => default

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def variance_v2(xs: Seq[Double]): Option[Double] =
    val m = mean(xs)
    val a = xs.map(x => math.pow(x - m.getOrElse[Double](0d), 2))
    mean(a)


}

@main
def main(): Unit = {
  import Option.*

  val x = None
  val y = x.orElse(Some(20))
  println(y)
}