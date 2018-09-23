def isPerfect(number: Int): Boolean = {
  def divisors(): List[Int] = {
    (2 to (number / 2)).foldLeft(List(1)) { case (l, x) =>
      if (number % x == 0)
        l :+ x
      else
        l
    }
  }

  divisors().sum == number
}

def findPerfectNumbers(max: Int): List[Int] = {
  (2 to max).foldLeft(List[Int]()) { case (l, x) =>
    if (isPerfect(x))
      l :+ x
    else
      l
  }
}

isPerfect(4)
findPerfectNumbers(10000)
