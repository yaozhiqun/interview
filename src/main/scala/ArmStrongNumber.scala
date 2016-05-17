object ArmStrongNumber extends App {

  def isArmStrong(num: Int): Boolean = {

    def digits(n: Int, l: List[Int]): List[Int] = {
      if (n / 10 == 0)
        n :: l
      else
        digits(n / 10, n % 10 :: l)
    }

    digits(num, Nil).reverse.map(x => x * x * x).sum == num
  }

  println(isArmStrong(371))
}
