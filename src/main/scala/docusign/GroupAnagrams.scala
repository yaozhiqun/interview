package docusign

object GroupAnagrams extends App {

  def solve(xs: List[String]): List[List[String]] = {
    xs.foldLeft(Map[String, List[String]]()) { case (m, s) =>
      def anagram(s1: String, s2: String): Boolean = {
        if (s1.length == s2.length) {
          def count(str: String): Map[Char, Int] = {
            str.toArray.foldLeft(Map[Char, Int]()) { (m, c) =>
              m + (c -> (m.getOrElse(c, 0) + 1))
            }
          }
          count(s1) == count(s2)
        } else {
          false
        }
      }

      m.find(e => anagram(e._1, s)) match {
        case Some(entry) => m + (entry._1 -> (s :: entry._2))
        case None => m + (s -> List(s))
      }
    }.values.toList
  }

  println(solve(List("at", "eat", "tea", "tan", "ate", "nat", "bat")))
}
