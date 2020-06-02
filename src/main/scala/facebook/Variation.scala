package facebook

object Variation extends App {

  def xvariations(xs: List[Int], n: Int): List[List[Int]] = {
    if (n > xs.length)
      Nil
    else if (n == 1)
      xs.map(List(_))
    else {
      xs match {
        case head :: tail =>
          mixmany(head, xvariations(tail, (n - 1))) ::: xvariations(tail, n)
        case Nil =>
          Nil
      }
    }
  }

  def mixmany(x: Int, xss: List[List[Int]]): List[List[Int]] = {
    xss match {
      case head :: tail => foldone(x, head) ::: mixmany(x, tail)
      case _ => Nil
    }
  }
  def foldone(x: Int, xs: List[Int]): List[List[Int]] = {
    (1 to xs.length).foldLeft(List(x :: xs)) { (l, i) => mixone(i, x, xs) :: l }
  }

  def mixone(i: Int, x: Int, xs: List[Int]): List[Int] =
    xs.slice(0, i) ::: (x :: xs.slice(i, xs.length))

  println(xvariations(List(1,2,3,4), 4).length)
  println(List(1,2,3,4).permutations.length)
}
