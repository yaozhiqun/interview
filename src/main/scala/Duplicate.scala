object Duplicate extends App {

  def duplicate(nums: List[Int]): Set[Int] = {
    nums.foldLeft(Map[Int, Int]()) { (map, num) =>
      map + (num -> (map.getOrElse(num, 0) + 1))
    }.filter(_._2 > 1).keys.toSet
  }
}
