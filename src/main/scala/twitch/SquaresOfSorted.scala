package twitch

object SquaresOfSorted extends App {

  val xs = List(-4,-1,0,3,10)

  def solve(xs: List[Int]): List[Int] = {
    def sort(xs: List[Int]): List[Int] = {
      xs match {
        case Nil => Nil
        case head :: _ =>
          val smaller = sort(xs.filter(_ < head))
          val pivot = xs.filter(_ == head)
          val higher = sort(xs.filter(_ > head))

          smaller ::: pivot ::: higher
      }
    }
    sort(xs.map(x => x * x))
  }

//  println(solve(xs))
//  println(xs.map(x => x * x).sorted)

  def sortedSquares(A: Array[Int]): Array[Int] = {
    var nonNegIndex = 0
    while(nonNegIndex < A.length && A(nonNegIndex) < 0) nonNegIndex += 1
    var i = nonNegIndex
    var j = nonNegIndex - 1
//    println(s"i: $i j: $j")
    Array.tabulate(A.length) { index =>
      if(j < 0 || i < A.length && A(i) < -A(j)) {
        val result = A(i) * A(i)
        i += 1
        println(s"1 $index: $result")
        result
      } else {
        val result = A(j) * A(j)
        j -= 1
        println(s"2 $index: $result")
        result
      }
    }
  }

  println(sortedSquares(xs.toArray).deep)

}
