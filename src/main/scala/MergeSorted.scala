object MergeSorted extends App {

  def combine(xs: List[Int], ys: List[Int], combined: List[Int] = List[Int]()): List[Int] = {
    xs match {
      case x :: xtail =>
        ys match {
          case y :: _ if y >= x => combine(xtail, ys, combined :+ x)
          case y :: ytail if y < x => combine(xs, ytail, combined :+ y)
          case Nil => combined ++ xs
        }
      case Nil =>
        combined ++ ys
    }
  }

  def merge(xss: List[List[Int]]): List[Int] = {
    xss match {
      case Nil => Nil
      case head :: Nil => head
      case head :: tail =>
        tail.foldLeft(head) { (l, xs) =>
          combine(l, xs)
        }
    }
  }

  println(merge(List(
    List(1,4,5),
    List(1,3,4),
    List(2,6)
  )))
//  println(combine(List(1,4,5), List(1,3,4)))
}
