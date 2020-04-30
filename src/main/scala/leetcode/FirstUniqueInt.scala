package leetcode

object FirstUniqueInt extends App {

  case class Stat(count: Int, firstIndex: Int)

  def find(xs: List[Int]): Option[Int] = {
    xs.zipWithIndex.foldLeft(Map[Int, Stat]()) { case (map, (x, index)) =>
        map + (x -> map.get(x).map(stat => stat.copy(count = stat.count + 1)).getOrElse(Stat(1, index)))
    }
  }.toList.filter(_._2.count == 1).sortBy(_._2.firstIndex).map(_._1).headOption

  def testFirstUniqueIntSpec(): Unit = {
    assert(find(List(2,3,5,5,2)) == Some(3))
    assert(find(List(2, 3, 5, 5, 2, 3)).isEmpty)
  }

  testFirstUniqueIntSpec()
}
