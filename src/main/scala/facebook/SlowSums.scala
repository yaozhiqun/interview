package facebook

object SlowSums extends App {

  def solve(xs: List[Int]): Option[Int] = {
    xs.sorted.reverse match {
//    sortAndReverse(xs) match {
      case Nil => None
      case first :: Nil => Some(first)
      case first :: second :: tail =>
        val result = tail.foldLeft((0, first + second)) { case ((sum, penalty), x) =>
          (sum + penalty, (penalty + x))
        }
        Some(result._1 + result._2)
    }
  }
  println(solve(List(4,2,1,3)))

  def sortAndReverse(xs: List[Int]): List[Int] = {
    xs match {
      case head :: _ =>
        val lower = sortAndReverse(xs.filter(_ < head))
        val pivot = xs.filter(_ == head)
        val higher = sortAndReverse(xs.filter(_ > head))
        higher ::: pivot ::: lower
      case _ =>
        xs
    }
  }
}
