package iterable

object NumberToEnglishSolution extends App {

  def transfer(x: Int): String = {

    val underTwenty = Map(
      0 -> "",
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine",
      10 -> "ten",
      11 -> "eleven",
      12 -> "twelve",
      13 -> "thirteen",
      14 -> "fourteen",
      15 -> "fifteen",
      16 -> "sixteen",
      17 -> "seventeen",
      18 -> "eighteen",
      19 -> "nineteen",
      20 -> "twenty"
    )

    val tens = Map(
      2 -> "twenty",
      3 -> "thirty",
      4 -> "forty",
      5 -> "fifty",
      6 -> "sixty",
      7 -> "seventy",
      8 -> "eighty",
      9 -> "ninety"
    )

    val tensPower = Map(
      3 -> "hundred",
      4 -> "thousand"
    )

    case class Node(n: Int, digits: Int) {
      override def toString: String = {
        digits match {
          case -1 => s"${underTwenty(n)}"
          case 1  => s"${underTwenty(n)}"
          case 2  => s"${tens(n)}"
          case 3 | 4 => s"${underTwenty(n)} ${tensPower(digits)}"
          case _ => throw new UnsupportedOperationException("too big digits")
        }
      }
    }

    def decompose(number: Int, round: Int = 1, nodes: List[Node] = Nil): List[Node] = {
      val divider = number / 10
      val remainder = number % 10

      divider match {
        case 0 => Node(remainder, round) :: nodes
        case other => decompose(other, round + 1, Node(remainder, round) :: nodes)
      }
    }

    val nodes = if (x <= 20) {
      List(Node(x, -1))
    } else if(x % 100 < 20) {
      decompose(x / 100, 3, List(Node(x % 100, -1)))
    } else {
      decompose(x)
    }

    nodes.mkString(" ")
  }

  println(transfer(1234))
  println(transfer(1230))
  println(transfer(1220))
  println(transfer(9219))
  println(transfer(219))
  println(transfer(10))
  println(transfer(20))
  println(transfer(11111))
}
