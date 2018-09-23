def toBin(x: Int): Int = {

  def rec(x: Int, xs: List[Int]): List[Int] = {
    if (x == 0)
      xs
    else
      rec(x / 2, x % 2 :: xs)
  }

  rec(x, Nil).mkString.toInt
}

toBin(100)

def toNum(x: Int): Int = {

  def rec(x: Int, times: Int, acc: Int): Int = {
    val divisor = x / 10
    val remainder = x % 10
    val newAcc = if (remainder == 1) acc + Math.pow(2, times).toInt else  acc

    if (divisor == 0)
      newAcc
    else
      rec(divisor, times + 1, newAcc)
  }

  rec(x, 0, 0)
}

toNum(1000)