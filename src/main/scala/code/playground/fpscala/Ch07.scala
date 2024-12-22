package code.playground.fpscala.ch07

import java.util.concurrent.*


object Par:

  opaque type Par[A] = ExecutorService => Future[A]

  extension[A] (pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  def unit[A](a: A): Par[A] =
    es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def sum[A](l: IndexedSeq[A]): Par[A] = ???

  //  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (a: A, b: B) => C): Par[C] = ???

  extension[A] (pa: Par[A]) def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call =
        println(s"thread running ${Thread.currentThread().getId}")
        a(es).get
    })


@main
def main(): Unit = {

  import Par.*

  def compute(n: Int): Int =
    println("computing...")
    if (n <= 1) 1 else n * compute(n - 1)

  val x: Par[Int] = Par.lazyUnit(compute(5))
  val threadPool = Executors.newFixedThreadPool(5)
  val y: Future[Int] = x.run(threadPool)
  println(s"value: ${y.get()}")
  threadPool.shutdown()
}