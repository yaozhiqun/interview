package rocketlawyer

object NthMostRare extends App {

  def find(xs: List[Int], nthRare: Int): Option[Int] = {
    xs match {
      case Nil => None
      case _ =>
        val map = xs.foldLeft(Map[Int, Int]()) { (m, x) =>
          m + (x -> (m.getOrElse(x, 0) + 1))
        }
        if (nthRare > map.size)
          None
        else
          Some(map.toList.sortBy(_._2).map(_._1).toList(nthRare - 1))
    }
  }

  println((find(List(5, 4, 3, 2, 1, 5, 4, 3, 2, 5, 4, 3, 5, 4, 5 ), 2)))
}
