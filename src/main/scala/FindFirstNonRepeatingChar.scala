object FindFirstNonRepeatingChar {

  def findFirstUniqueChar(str: String): Option[Char] = {

    case class Stat(count: Int, idx: Int)

    str.toCharArray.zipWithIndex.foldLeft(Map[Char, Stat]()) { case (map, (c, index)) =>
      map + (c -> map.get(c).map(s => s.copy(count = s.count + 1)).getOrElse(Stat(1, index)))
    }.toList
      .sortBy { case (c, stat) => stat.idx }
      .collectFirst { case (c, stat) if stat.count == 1 => c }
  }
}
