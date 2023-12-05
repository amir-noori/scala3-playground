package code.playground.scala3

import scala.annotation.tailrec

object Syntax {


  // if expression
  val one = 1
  val two = 2
  var counter = 0

  val comparision_v2 = if (two > one) "bigger" else "smaller"
  val comparision_v3 = if two > one then "bigger" else "smaller"
  val compariosionBlock =
    if two > one then
    // do sth
      "bigger"
    else
    // do sth else
      "smaller"


  // while expression
  val whileValue: Unit = while counter < 10 do
    println("all right")
    counter = counter + 1


  // for comprehension
  for
    a <- List(1, 2, 3)
    b <- List("a", "b", "c")
  yield s"$a -> $b"


  // function
  def fib(n: Int): Int =
    @tailrec
    def fibTailrec(m: Int, r_1: Int, r_2: Int, r: Int): Int =
      if m == n then r
      else if m == 0 then fibTailrec(m + 1, 1, 0, 1)
      else if m == 1 then fibTailrec(m + 1, 1, 1, 1)
      else fibTailrec(m + 1, r, r_1, r + r_1)

    fibTailrec(0, 0, 0, 0)
  end fib

  // classes
  class Box:
    def open(): Unit =
      println("openning")

    def close(): Unit =
      println("closing")
  end Box

  @main
  def main(): Unit = {
    (1 to 10).map(fib).foreach(println)
  }


}
