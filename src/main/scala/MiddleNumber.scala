object MiddleNumber extends App {

  def find(xs: List[Int]): Option[Int] = {

    def rec(xs: List[Int], leftSum: Int, index: Int): Option[Int] = {
      xs match {
        case head :: tail =>
          if (head + leftSum == tail.sum)
            Some(index)
          else
            rec(tail, head + leftSum, index + 1)
        case _ => None
      }
    }

    rec(xs, 0, 0)
  }

  println(find(List(1, 14, 2, 8, 4, 5, 12, 45, 25, 1, 27, 26, 3, 34, 8, 9, 30, 32, 2)))
}