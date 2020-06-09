package facebook

object PairSums extends App {
  def numberOfWays(arr: Array[Int], k: Int) : Int = {
    @scala.annotation.tailrec
    def recur(xs: List[Int], pairs: List[(Int, Int)] = Nil): List[(Int, Int)] = {
      xs match {
        case head :: tail =>
          recur(tail, tail.foldLeft(pairs) {
            case (ps, x) if (head + x) == k => (head, x) :: ps
            case (ps, _) => ps
          })
        case Nil =>
          pairs
      }
    }
//    println(recur(arr.toList))
    recur(arr.toList).size
  }

  println(numberOfWays(Array(1, 5, 3, 3, 3), 6))
//  println(Array(1, 5, 3, 3, 3).sliding(2).count(e => e.sum == 6))
//  Array(1, 5, 3, 3, 3).sliding(2).filter(_.sum == 6).toList.foreach(e => println(e.deep))
}
