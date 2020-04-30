package leetcode

object LongestCommonSub extends App {

  def find(str1: String, str2: String): Option[String] = {
    val (longer, shorter) = if (str1.length > str2.length) (str1, str2) else (str2, str1)

    def recur(sub: String, longer: String, shorter: String, matched: Array[Char] = Array.emptyCharArray): Option[String] = {
      if (matched.mkString == sub) {
        Some(sub)
      } else if (longer.isEmpty || shorter.isEmpty) {
        None
      } else {
        longer.toCharArray.foreach { c1 =>
          shorter.toCharArray.foreach { c2 =>
            if (c1 == c2) {
              return recur(sub, longer.tail, shorter.tail, matched :+ c1)
            }
          }
        }
        None
      }
    }

    shorter.inits.flatMap(_.tails.toList.init).toList.sortBy(_.length).reverse.foreach { sub =>
      val found = recur(sub, longer, shorter)
      if (found.nonEmpty)
        return found
    }
    None
  }

  println(find("abcde", "afp"))
  println(find("abc", "abc"))
  println(find("ace", "dfs"))

  def lcs(s1: String, s2: String): Option[String] = {

    def rec(l1: List[Char], l2: List[Char]): List[Char] = {
      (l1, l2) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (x :: xs, y :: ys) => {
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
