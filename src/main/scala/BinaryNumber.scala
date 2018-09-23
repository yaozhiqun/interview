object BinaryNumber extends App {

  def toBinary(number: Int): Array[Int] = {

    def recurse(num: Int, binary: Array[Int]): Array[Int] = {
      if (num == 0)
        binary
      else
        recurse(num / 2, num % 2 +: binary)
    }
    recurse(number, Array[Int]())
  }

  println(toBinary(25).mkString)

  def toNumber(binary: Int): Int = {

    def recurse(a: List[Int], n: Int): Int = {
      a match {
        case 0 :: Nil => n
        case 1 :: Nil => n + 1
        case 0 :: tail => recurse(tail, n)
        case 1 :: tail => recurse(tail, n + Math.pow(2, tail.length).toInt)
        case _ => throw new IllegalArgumentException(s"$binary is not a binary number")
      }
    }

    recurse(binary.toString.toCharArray.map(_.asDigit).toList, 0)
  }

  println(toNumber(101))

  def toNumber2(binary: Int) = {

    def recurse(b: Int, list: List[Int]): List[Int] = {
      val y = b / 10
      val x = b % 10 :: list
      if (y == 0) x
      else recurse(y, x)
    }

    val list = recurse(binary, Nil)
    list.foldLeft((list.length - 1, 0): (Int, Int)) { case ((n, acc), num) =>
      val x = num match {
        case 0 => acc
        case 1 => acc + Math.pow(2, n).toInt
        case _ => throw new IllegalArgumentException(s"$binary is not a binary number")
      }
      (n - 1, x)
    }._2
  }
  println(toNumber2(110))

  def toNumber3(binary: Int): Int = {

    def recurse(num: Int, times: Int, acc: Int): Int = {
      val divisor = num / 10
      val remainder = num % 10
      val newAcc = if (remainder == 1) acc + Math.pow(2, times).toInt else acc

      if (divisor == 0) newAcc
      else recurse(divisor, times + 1, newAcc)
    }

    recurse(binary, 0, 0)
  }

  println(toNumber3(111111))

  Array(1, 0, 1).mkString.toInt
}
