object PerfectNumber extends App {

  def findDivisors(num: Int): List[Int] = {
    (1 until num).filter(num % _ == 0).toList
  }

  def isPerfectNumber(num: Int, divisors: Map[Int, List[Int]]): Boolean = {
    divisors(num).reverse match {
      case head :: tail => tail.sum == head
      case _ => false
    }
  }

  def isEven: Int => Boolean = _ % 2 == 0

  def findPerfectNumber(limit: Int): Array[Int] = {
    val divisors = (1 until limit).filter(isEven).foldLeft(Map[Int, List[Int]]()) { case (map, int) =>
      map + (int -> map.getOrElse(int, findDivisors(int)))
    }

    (1 until limit).filter(isEven).par.filter(isPerfectNumber(_, divisors)).toArray
  }

  val perfects = findPerfectNumber(1000)
  perfects.foreach(println)

}
