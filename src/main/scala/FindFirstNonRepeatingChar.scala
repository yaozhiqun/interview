

object FindFirstNonRepeatingChar extends App {

  case class Stat(count: Int, index: Int)

  def firstNonRepeat(str: String): Option[Char] = {
    str.toArray.zipWithIndex.foldLeft(Map[Char, Stat]()) { case (map, (char, index)) =>
      map + (char -> map.get(char).map(stat => stat.copy(count = stat.count + 1, index = index)).getOrElse(Stat(1, index)))
    }.filter(_._2.count == 1).toList.sortBy(_._2.index).headOption.map(_._1)
  }

  println(firstNonRepeat("lloword"))
  println(firstNonRepeat("llo  wworld"))
  println(firstNonRepeat("hello  rwworldhed"))
}
