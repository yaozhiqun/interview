package twitch

object FirstMissingPositive extends App {

  def find(xs: List[Int]): Int = {
    xs.foldLeft(1) { (first, x) =>
      if (x == first)
        first + 1
      else
        first
    }
  }

  println(find(List(1,2,0)))
  println(find(List(2,1,-2,0))) // 3
  println(find(List(3,-1,1)))
  println(find(List(7,8,9,11,12)))
  println(find(List(1,2,1,7,8,9,11,12)))
  println(find(List(1,2,3,4,5,6,7,8,9)))
  println(find(List(1,2,3,4,5,6,7,18,9)))
}
