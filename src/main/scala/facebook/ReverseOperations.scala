package facebook

object ReverseOperations extends App {

  def solve(xs: List[Int]): List[Int] = {
    xs.zipWithIndex.foldLeft((List[Int](), Option.empty[Int])) {
      case ((l, None), (x, i)) if i % 3 == 0 => (l :+ x, None)
      case ((l, None), (x, _)) => (l, Some(x))
      case ((l, Some(last)), (x, _)) => (l :+ x :+ last, None)
    }._1
  }

  println(solve(List(1, 2, 8, 9, 12, 16)))
  println(solve(List(1, 2, 8, 9, 12, 16, 3)))
}
