package code.playground.fpscala.ch04

import scala.annotation.tailrec


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

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    oa => oa.map(f)

  def lift[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
    (oa, ob) => map2(oa, ob)(f)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.flatMap(y => Some(f(x, y)))) // meh

  def map2_v2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    lift(f.curried)(ao).flatMap(h => bo.map(b => h(b)))

  def map2_v3[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](la: List[Option[A]]): Option[List[A]] =
    la.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2_v3(a, acc)((x, y) => x :: y))

  def sequence_v2[A](la: List[Option[A]]): Option[List[A]] =
    traverse(la)(oa => oa)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, olb) => map2_v3(f(a), olb)((x, lb) => x :: lb))

  def traverse_v2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(ls: List[A], acc: List[B]): Option[List[B]] = {
      ls match
        case x :: xs => f(x) match
          case Some(v) => go(xs, v :: acc)
          case None => None
        case Nil => acc match
          case Nil => None
          case _ => Some(acc)
    }

    go(as, Nil)
  }

  def traverse_v3[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a, acc) =>
      map2(f(a), acc)(_ :: _))

}

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)

  def flatMap[Er >: E, B](f: A => Either[Er, B]): Either[Er, B] = this match
    case Right(a) => f(a)
    case Left(e) => Left(e)

  def orElse[Er >: E, B >: A](b: => Either[Er, B]): Either[Er, B] = this match
    case Left(e) => b
    case Right(a) => Right(a)

  def map2[Er >: E, B, C](that: Either[Er, B])(f: (A, B) => C): Either[Er, C] =
    this.flatMap(x => that.map(y => f(x, y)))

  def map2_v2[Er >: E, B, C](that: Either[Er, B])(f: (A, B) => C): Either[Er, C] =
    for {
      x <- this
      y <- that
    } yield f(x, y)


object Either {
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right[E, List[B]](Nil))((a, lb) => f(a).map2(lb)((x, y) => x :: y))

}


@main
def main(): Unit = {
  import Option.*

  val x = None
  val y = x.orElse(Some(20))
  println(y)

  val l = List(List(10), List(20), List(30))
  val a = l.map(x => x)
  val b = l.flatMap(x => x)
  println(s"a ${a}")
  println(s"b ${b}")

  val nums = List(Some(10), Some(20), None, Some(30), Some(40), Some(50))
  val numsResult = Option.sequence(nums)
  println(s"numsResult: ${numsResult}")


}