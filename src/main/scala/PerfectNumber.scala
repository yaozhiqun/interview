object PerfectNumber extends App {

  def findPerfect(x: Int): List[Int] = {
    (1 to x).par.foldLeft(List[Int]()) { (l, n) =>
      if ((1 until n).filter(n % _ == 0).sum == n)
        l :+ n
      else
        l
    }
  }

  findPerfect(1000) foreach println
}
