package leetcode

object LongestCommonSub extends App {

  def lcs(s1: String, s2: String): Option[String] = {

    def rec(l1: List[Char], l2: List[Char]): List[Char] = {
      (l1, l2) match {
        case (_, Nil) | (Nil, _) => Nil
        case (x :: xs, y :: ys) =>
          if (x == y) {
            x :: rec(xs, ys)
          } else {
            val xInYs = rec(x :: xs, ys)
            val yInXs = rec(xs, y :: ys)
            if (xInYs.length > yInXs.length) {
              xInYs
            } else {
              yInXs
            }
          }
      }
    }

    val result = rec(s1.toCharArray.toList, s2.toCharArray.toList)
    if (result.isEmpty)
      None
    else
      Some(result.mkString)
  }

  println(lcs("abcde", "afp"))
  println(lcs("abc", "abc"))
  println(lcs("ace", "dfs"))
}
