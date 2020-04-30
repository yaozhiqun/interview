object BinaryNumber extends App {

  def toBinary(n: Int): List[Int] = {

    def rec(x: Int, xs: List[Int]): List[Int] = {
      if (x == 0)
        xs
      else
        rec(x / 2, x % 2 :: xs)
    }

    rec(n, Nil)
  }

  println(toBinary(25).mkString)

  def toBin(dec: Int): Int = {
    def recur(x: Int, bin: List[Int]): List[Int] = {
      val divider = x / 2
      val remainder = x % 2

      val xs = remainder :: bin
      if (divider > 0) {
        recur(divider, xs)
      } else {
        xs
      }
    }

    val l = recur(dec, Nil)
    l.zipWithIndex.foldLeft(0) {
      case (acc, (n, index)) => acc + n * Math.pow(10, l.size - index - 1).toInt
    }
  }

  println(toBin(96))

  def toDec(bin: Int): Int = {
    def recur(bin: Int, dec: Int, power: Int): Int = {
      val divider = bin / 10
      val remainder = bin % 10

      val newDec = dec + (if (remainder == 1) Math.pow(2, power).toInt else 0)

      if (divider == 0) {
        newDec
      } else {
        recur(divider, newDec, power + 1)
      }
    }

    recur(bin, 0, 0)
  }

  println(toDec(toBin(96)) == 96)
}
