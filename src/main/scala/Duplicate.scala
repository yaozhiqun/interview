object Duplicate extends App {

  val duplicate = List(1, 2, 3, 4, 5, 3, 2, 3, 4, 5, 6, 4, 5).foldLeft(Map[Int, Int]()) { case (map, int) =>
    map + (int -> (map.getOrElse(int, 0) + 1))
  }.filter(_._2 > 1).keys

  println(duplicate)
}
