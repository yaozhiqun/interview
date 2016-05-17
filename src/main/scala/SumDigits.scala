object SumDigits extends App {

  def sum(n: Int): Int = {
    val x = n / 10
    if (x == 0)
      n
    else
      (n % 10) + sum(x)
  }

  println(sum(123))
}
