package facebook

object SmallestKth extends App {

  def combine(xs: List[Int], ys: List[Int], combined: List[Int] = List[Int]()): List[Int] = {
    xs match {
      case x :: xtail =>
        ys match {
          case y :: _ if y > x => combine(xtail, ys, combined :+ x)
          case y :: ytail if y < x => combine(xs, ytail, combined :+ y)
          case Nil => combined ++ xs
        }
      case Nil =>
        combined ++ ys
    }
  }

  println(combine(List(1,2,5,6), List(3,4,7)))

//  def find(xss: List[List[Int]], kth: Int): Option[Int] = {
//    def recur(xss: List[List[Int]], kth: Int = 1): Option[Int] = {
//      xss match {
//        case (x :: xs) :: yss =>
//          yss.foldLeft((x, List(xs))) {
//            case ((smallest, ll), y :: ys) =>
//              if (y > smallest)
//                (smallest, (y :: ys) :: ll)
//              else
//                (y, (x :: xs) :: ll)
//          }
//      }
//      xss.foldLeft(Option.empty[Int]) {
//        case (None, head :: tail) =>
//
//      }
//    }
//  }
}
