object SingleNumber extends App {

  def single(numbers: Array[Int]): Option[Int] = {
    numbers.foldLeft(Map[Int, Int]()) { case (map, e) =>
      map + (e -> (map.getOrElse(e, 0) + 1))
    }.find(_._2 == 1).map(_._1)
  }

  def sn(nums: Array[Int]): Int = {
    var res: Int = 0
    nums.foreach { n =>
      res ^= n
    }
    res
  }

  val nums = Array(1, 1, 2, 3, 4, 4, 2)
  println(single(nums))
  println(sn(nums))
}
