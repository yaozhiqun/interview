import java.util.concurrent._

import scala.collection.parallel.ForkJoinTaskSupport

object PrimeFinder extends App {

  def isPrime(n: Int): Boolean = {
    !(2 until Math.sqrt(n).toInt).exists(n % _ == 0)
  }

  val target = 1000
  val start = System.nanoTime()
  val numbers = (3 until target).toList

  def execute(): Unit = {
    val executor = Executors.newFixedThreadPool(20)
    val totalPrimes = numbers.grouped(10).toList.map {
      list => executor.submit(
        new Callable[Int] {
          override def call(): Int = list.count(isPrime)
      })
    }.map(_.get).sum
    println(s"There are $totalPrimes prime numbers to $target")
  }

  def forkJoin() = {
    val groups = numbers.grouped(10).toList.par
    groups.tasksupport = new ForkJoinTaskSupport()
    val totalPrimes = groups.toList.map { l =>
      l.count(isPrime)
    }.sum
    println(s"There are $totalPrimes prime numbers to $target")
  }

  def par(): Unit = {
    val totalPrimes = numbers.par.count(isPrime)
    println(s"There are $totalPrimes prime numbers to $target")
  }

  par()
  val end = System.nanoTime()
  println(s"It took ${(end -  start) / 1.0e9} seconds")
}

object Solution extends App {

}