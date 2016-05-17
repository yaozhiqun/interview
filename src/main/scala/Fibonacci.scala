import scala.annotation.tailrec

object Fibonacci extends App {

  def fib(n: Int): Long = {

    @tailrec
    def recurse(prev: Int, curr: Int, n: Int): Long = {
      n match {
        case x if x <= 2 => curr
        case x => recurse(curr, curr + prev, x - 1)
      }
    }

    recurse(1, 1, n)
  }

  println(fib(8))

//  1, 1, 2, 3, 5, 8, 13
//  1, 2, 3, 4, 5, 6, 7
//
//  f(7) = f(6) + f(5)
//  f(6) = f(5) + f(4)
//  f(5) = f(4) + f(3)
//  f(4) = f(3) + f(2)


  def fib2(n: Int): Int = {

    @tailrec
    def recurse(n: Int, lo: Int, hi: Int): Int = {
      if (n <= 2) hi
      else recurse(n - 1, hi , lo + hi)
    }
    recurse(n, 1, 1)
  }

  println(fib2(8))
}
