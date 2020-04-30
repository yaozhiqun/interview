object ArmStrongNumber extends App {

  def isArmStrong(num: Int): Boolean = {

    def digits(n: Int): List[Int] = {
      def split(x: Int, xs: List[Int]): List[Int] = {
        if (x / 10 == 0)
          x :: xs
        else
          split(x / 10, x % 10 :: xs)
      }
      split(n, Nil)
    }


    println(digits(num))

    digits(num).map(x => x * x * x).sum == num
  }

  println(isArmStrong(371))
}
