package sort

object MergeSort extends App {

  def sort(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => xs
      case _ :: Nil => xs
      case _ =>
        def recur(xs: List[Int], ys: List[Int]): List[Int] = {
          (xs, ys) match {
            case (_, Nil) => xs
            case (Nil, _) => ys
            case (x :: xtail, y :: ytail) =>
              if (x > y)
                x :: recur(xtail, ys)
              else
                y :: recur(xs, ytail)

          }
        }
        val (left, right) = xs splitAt (xs.length / 2)
        recur(left, right)
    }
  }

  println(sort(List(4,2,3,1)))
}
