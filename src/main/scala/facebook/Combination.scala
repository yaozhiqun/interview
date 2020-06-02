package facebook

object Combination extends App {
  def xcombinations(l: List[Int], n: Int): List[List[Int]] =
    if (n > l.length)
      Nil
    else if (n == 1)
      l.map(List(_))
    else
      l match {
        case head :: tail =>
          xcombinations(tail, (n - 1)).map(head :: _) ::: xcombinations(tail, n)
        case _ => Nil
      }

  println(xcombinations(List(1, 2, 3, 4), 4))
}
