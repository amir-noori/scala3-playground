package code.playground.fpscala.ch03

import scala.annotation.tailrec


enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List {
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))

  def sum(l: List[Int]): Int = l match
    case Nil => 0
    case Cons(h, t) => h + sum(t)

  def sumViaFoldRight(l: List[Int]): Int = foldLeft(l, 0, (a, b) => a + b)

  def product(l: List[Double]): Double = l match
    case Nil => 1
    case Cons(h, t) =>
      if h == 0.0 then 0
      else h * product(t)

  // this version of product cannot halt encountering value 0
  def productViaFoldRight(l: List[Int]): Int = foldLeft(l, 1, _ * _)

  def tail[A](l: List[A]): List[A] = l match
    case Cons(h, t) => t
    case Nil => sys.error("tail of empty list")

  def head[A](l: List[A]): A = l match
    case Cons(h, t) => h
    case Nil => sys.error("tail of empty list")

  def setHead[A](l: List[A], a: A): List[A] = l match
    case Cons(h, t) => Cons(a, t)
    case Nil => sys.error("set head of empty list")

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match
      case Cons(h, t) => drop(t, n - 1)
      case Nil => sys.error("drop from empty list")

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) => if f(h) then dropWhile(l, f) else l
    case _ => l

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match
    case Cons(h, t) => Cons(h, append(t, l2))
    case Nil => l2

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2, (a, acc) => Cons(a, acc))

  def appendViaFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(reverse(l1), l2, (a, acc) => Cons(a, acc))

  def init[A](l: List[A]): List[A] = l match
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => sys.error("cannot get Nil init")

  def reverse[A](l: List[A]): List[A] = {
    @tailrec
    def go(as: List[A], acc: List[A]): List[A] = {
      as match
        case Nil => acc
        case Cons(h, t) => go(t, Cons(h, acc))
    }

    go(l, Nil)
  }

  def reverseViaFoldLeft[A](l: List[A]): List[A] =
    foldLeft[A, List[A]](l, Nil, (a, b) => Cons(a, b))

  def init_v2[A](l: List[A]): List[A] = {
    @tailrec
    def go(as: List[A], acc: List[A]): List[A] = {
      as match
        case Cons(_, Nil) => acc
        case Cons(h, t) => go(t, Cons(h, acc))
        case Nil => sys.error("cannot get Nil init")
    }

    reverse(go(l, Nil))
  }

  def foldRightViaFoldLeft_x[A, B](as: List[A], acc: B, f: (A, B) => B): B =
  /*
    (b: B) => b is the identity function as the accumulator
   */
    foldLeft(as, (b: B) => b, (a: A, b_b: B => B) => b => b_b(f(a, b)))(acc)

  def foldRight[A, B](l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z, f))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B, f: (A, B) => B): B =
    foldLeft(reverseViaFoldLeft(l), z, f)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z), f)

  def len[A](l: List[A]): Int = foldLeft(l, 0, (_, b) => b + 1)

  def concat[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A], appendViaFoldLeft)

  def inc(l: List[Int]): List[Int] =
    foldLeft[Int, List[Int]](reverse(l), Nil: List[Int], (i, acc) => Cons(i + 1, acc))

  def doubleToStr(l: List[Double]): List[String] =
    foldRight[Double, List[String]](l, Nil: List[String], (i, acc) => Cons(s"*${i}*", acc))

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]](l, Nil: List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](l: List[A], f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

  def filterViaFlatMap[A](l: List[A], f: A => Boolean): List[A] =
    flatMap(l, a => if f(a) then List(a) else Nil)

  def flatMap[A, B](l: List[A], f: A => List[B]): List[B] =
    foldRight[A, List[B]](l, Nil: List[B], (a, acc) => append(f(a), acc))

  def mergeLists[A, B, C](l1: List[A], l2: List[B], f: (A, B) => C): List[C] =
    (l1, l2) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), mergeLists(t1, t2, f))
      case (Nil, _) => Nil
      case (_, Nil) => Nil

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

    @tailrec
    def go(as: List[A], s: List[A], stillMatching: Boolean, from: List[A]): Boolean = {
      (as, s) match
        case (Cons(h1, t1), Cons(h2, t2)) =>
          if (h1 != h2) {
            if (stillMatching)
              go(tail(from), sub, false, tail(from))
            else
              go(t1, s, false, t1)
          } else {
            go(t1, t2, true, from)
          }
        case (Nil, Nil) => stillMatching
        case (Nil, _) => false
        case (_, Nil) => true
    }

    go(l, sub, false, l)
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false

  @annotation.tailrec
  def hasSubsequence_v2[A](sup: List[A], sub: List[A]): Boolean = sup match
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence_v2(t, sub)

}



@main
def main(): Unit = {
  val l: List[Int] = List(1, 2, 3, 4)
  val ld: List[Double] = List(1, 2, 3, 4)
  val l2: List[Int] = List(10, 20, 30, 40)
  val ll: List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  println("list sum: " + List.sumViaFoldRight(l))
  println("list product: " + List.productViaFoldRight(l))
  println("list init: " + List.init_v2(l))
  println("list len: " + List.len(l))
  println("list reverseViaFoldLeft: " + List.reverseViaFoldLeft(l))
  println("list appendViaFoldLeft: " + List.appendViaFoldLeft(l, l2))
  println("list appendViaFoldRight: " + List.appendViaFoldRight(l, l2))
  println("list concat: " + List.concat(ll))
  println("list inc: " + List.inc(l))
  println("list doubleToStr: " + List.doubleToStr(ld))
  println("list hasSubsequence: " + List.hasSubsequence(l, List(1, 2)))
  println("list hasSubsequence: " + List.hasSubsequence(l, List(2, 3)))
  println("list hasSubsequence: " + List.hasSubsequence(l, List(3, 4)))
  println("list hasSubsequence: " + List.hasSubsequence(l, List(1, 3)))
  println("list hasSubsequence: " + List.hasSubsequence(l, List(2, 4)))
}