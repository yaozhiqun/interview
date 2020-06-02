package facebook

object LargestTriples extends App {

  def solve(xs: List[Int]): List[Int] = {
    xs match {
      case Nil =>
        Nil
      case _ :: Nil =>
        List(-1)
      case _ :: _ :: Nil =>
        List(-1, -1)
      case first :: second :: third :: Nil =>
        List(-1, -1, first * second * third)
      case first :: second :: third :: tail =>
        val sorted = List(first, second, third).sorted.reverse
        tail.foldLeft(((sorted(0), sorted(1), sorted(2))), List(-1, -1, (first * second * third))) {
          case ((largest, list), x) if x > largest._3 =>
            ((largest._2, largest._3, x), list :+ (largest._2 * largest._3 * x))
          case ((largest, list), x) if x > largest._2 & x < largest._3 =>
            ((largest._2, x, largest._3), list :+ (largest._2 * largest._3 * x))
          case ((largest, list), x) if x > largest._1 & x < largest._2 =>
            ((x, largest._2, largest._3), list :+ (largest._2 * largest._3 * x))
          case ((largest, list), x) if x < largest._1 =>
            (largest, list :+ (largest._1 * largest._2 * largest._3))
        }._2
    }
  }

  println(solve(List(1, 2, 3, 4, 5)))
  println(solve(List(2, 1, 2, 1, 2)))
}
