def reverse(number: Int): Int = {

  def rec(number: Int, reversed: Int = 0): Int = {
    val divisor = number / 10
    val remainder = number % 10

    if (divisor == 0) {
      reversed + remainder
    } else {
      rec(divisor, (reversed + remainder) * 10)
    }
  }

  rec(number)
}

reverse(-12345678)