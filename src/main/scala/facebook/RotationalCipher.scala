package facebook

object RotationalCipher extends App {

  def solve(str: String, factor: Int): String = {
    val alphsUpper = ('A' to 'Z')
    val alphsLower = ('a' to 'z')
    val numbers = ('0' to '9')

    def rotate(n: Int, range: Range): Int = {
      val m = n + factor
      m match {
        case x if x > range.max => range.min + (m - range.max - 1)
        case x if x >= range.min & x < range.max => x
      }
    }

    str.toCharArray.foldLeft(Array[Char]()) { (newStr, char) =>
      newStr :+ {
        char match {
          case c if alphsLower.contains(c) => rotate(c.toInt, Range(alphsLower.head.toInt, alphsLower.last.toInt + 1)).toChar
          case c if alphsUpper.contains(c) => rotate(c.toInt, Range(alphsUpper.head.toInt, alphsUpper.last.toInt + 1)).toChar
          case n if numbers.contains(n) => rotate(n.toInt, Range(numbers.head.toInt, numbers.last.toInt + 1)).toChar
          case other => other
        }
      }
    }.mkString
  }

  println(solve("Zebra-493?", 3))
}
