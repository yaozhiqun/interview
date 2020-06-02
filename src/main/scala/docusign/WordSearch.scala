package docusign

import scala.collection.mutable.ListBuffer

object WordSearch extends App {

  case class Dot(value: Char, x: Int, y: Int)

  def solve(xss: Array[Array[Char]], str: String): Boolean = {
    val dots = ListBuffer[Dot]()
    for {
      row <- xss.indices
      col <- xss(row).indices
    } {
      dots += Dot(xss(row)(col), row, col)
    }

    def search(cs: List[Char], path: List[(Int, Int)] = List((-1, -1))): Boolean = {
      cs match {
        case Nil => true
        case head :: tail =>
          dots.toList.filter { dot => (dot.value == head) } exists { dot =>
              !path.contains((dot.x, dot.y)) &&
              ((Math.abs(dot.x - path.last._1) == 1) || (Math.abs(dot.y - path.last._2) == 1)) &&
              search(tail, path :+ (dot.x, dot.y))
          }
      }
    }

    str.toArray.toList match {
      case Nil => false
      case other =>
        search(other)
    }
  }

  val array = Array(
    Array('A','B','C','E'),
    Array('S','F','C','S'),
    Array('A','D','E','E')
  )
  println(solve(array, "ABCCED"))
  println(solve(array, "SEE"))
  println(solve(array, "ABCB"))

}
