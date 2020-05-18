package leetcode

object ThreeSumSolution extends App {

  def sum3(xs: List[Int]): List[(Int, Int, Int)] = {

    xs.zipWithIndex.foldLeft(List[(Int, Int, Int)]()) { case (l3, (x, xi)) =>

      def sum2(ys: List[Int], s2: Int): List[(Int, Int)] = {
        ys.zipWithIndex.foldLeft(List[(Int, Int)]()) { case (l2, (y, yi)) =>
          ys.takeRight(ys.length - yi - 1).find(_ == s2 - y) match {
            case Some(n) => l2 :+ (y, n)
            case _ => l2
          }
        }
      }


      l3 ::: sum2(xs.takeRight(xs.length - xi - 1), -x).map(tuple => (x, tuple._1, tuple._2))
    }
  }

  def sum3_(xs: List[Int]): List[(Int, Int, Int)] = {

    xs match {
      case x :: xTail =>
        def sum2(ys: List[Int], sum: Int): List[(Int, Int)] = {
          ys match {
            case y :: yTail => yTail.find(_ == sum - y).map((y, _) :: sum2(yTail, sum)).getOrElse(sum2(yTail, sum))
            case _ => Nil
          }
        }
        sum2(xTail, -x).map(t => (x, t._1, t._2)) ::: sum3_(xTail)
      case _ =>
        Nil
    }
  }

  println(sum3(List(-1, 0, 1, 2, -1, -4)))
  println(sum3_(List(-1, 0, 1, 2, -1, -4)))
}
