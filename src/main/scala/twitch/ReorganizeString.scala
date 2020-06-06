package twitch

// https://leetcode.com/problems/reorganize-string/
object ReorganizeString extends App {

  def re(str: String): String = {
    val length = str.length
    val charMap = str.toArray.foldLeft(Map[Char, Int]()) { (m, char) =>
      m + (char -> (m.getOrElse(char, 0) + 1))
    }

    val sorted = charMap.toList.sortBy(_._2).reverse
    if (sorted.head._2 > (length + 1) / 2)
      ""
    else {
      val part = sorted.flatMap { elem => List.fill(elem._2)(elem._1) }
      val (x, y) =
        if (length % 2 == 0)
          part.splitAt(length / 2)
        else
          part.splitAt(length / 2 + 1)
      val zipped = (x zip y)
      val p = (zipped.flatMap { x => List(x._1, x._2) }).mkString

      if(x.size > y.size)
        p + x.head
      else
        p
    }
//      def compose(chars: List[(Char, Int)], init: String): String = {
//        char
//        init.toArray.foldLeft("") { (s, c) =>
//
//        }
//      }
//      val (chars, init) = sorted.foldLeft((List[(Char, Int)](), "")) { case ((cs, s), (c, n)) =>
//        if (n > 1)
//          (((c, n - 1) :: cs), s + c)
//        else
//          (cs, s + c)
//      }
//      compose(chars, init)
  }

  println(re("aabbcd"))
//  println(re("aaab"))
}
