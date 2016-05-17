object FindFirstNonRepeatingChar extends App {

  def findFirstUniqueChar(str: String): Option[Char] = {

    case class Stat(count: Int, idx: Int)

    str.toCharArray.zipWithIndex.foldLeft(Map[Char, Stat]()) { case (map, (char, idx)) =>
      val stat = map.getOrElse(char, Stat(0, idx))
      val newStat = Stat(stat.count + 1, stat.idx)
      map + (char -> newStat)
    }.toList
      .sortBy { case (_, s) => s.idx }
      .find { case (_, s) => s.count == 1 }
      .map { case (c, _) => c }
  }

  findFirstUniqueChar("hello world").foreach(println)
}
