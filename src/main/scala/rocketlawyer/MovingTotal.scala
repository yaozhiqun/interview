package rocketlawyer

object MovingTotal extends App {

  def exists(xs: List[Int], target: Int): Boolean = {
    xs match {
      case Nil => false
      case _ :: Nil => false
      case _ :: _ :: Nil => false
      case first :: second :: third :: Nil => (first + second + third) == target
      case first :: second :: third :: tail =>
        tail.foldLeft((first, second, third), first + second + second) { case (((_, x2, x3), total), x) =>
          if (total == target)
            return true
          else
            ((x2, x3, x), x2 + x3 + x)
        }._2 == target
    }
  }

  println(exists(List(1,2,3), 6))
  println(exists(List(1,2,3,4), 9))
  println(exists(List(1,2,3,4), 7))
  println(exists(List(1,2,3,4,5), 12))
}
