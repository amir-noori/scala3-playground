package code.playground.fpscala


enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(l: List[Int]): Int = l match
      case Nil => 0
      case Cons(h, t) => h + sum(t)

  def product(l: List[Double]): Double = l match
    case Nil => 1
    case Cons(h, t) =>
      if h == 0.0 then 0
      else h * product(t)


@main
def main(): Unit = {
  println("")
}