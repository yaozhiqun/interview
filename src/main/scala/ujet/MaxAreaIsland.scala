package ujet

import scala.collection.mutable.ListBuffer

object MaxAreaIsland extends App {

  case class Dot(row: Int, col: Int)
  case class Island(dots: List[Dot])

  def islands(xys: Array[Array[Int]]): List[Island] = {

    def findLeft(islands: ListBuffer[Island], dot: Dot): Option[Island] = {
      islands.find(_.dots.exists(d => d.col + 1 == dot.col && d.row == dot.row))
    }

    def findUpper(islands: ListBuffer[Island], dot: Dot): Option[Island] = {
      islands.find(_.dots.exists(d => d.col == dot.col && d.row + 1 == dot.row))
    }

    val islands = ListBuffer[Island]()
    for {
      row  <- xys.indices
      col <- xys(row).indices
    } {
      if (xys(row)(col) == 1) {
        val dot = Dot(row, col)
        val left = findLeft(islands, dot)
        val upper = findUpper(islands, dot)
        if (left.isDefined && upper.isDefined) {
          islands -= left.get
          islands -= upper.get
          islands += Island((dot :: left.get.dots ::: upper.get.dots).distinct)
        } else if (left.isDefined) {
          islands -= left.get
          islands += Island(dot :: left.get.dots)
        } else if (upper.isDefined) {
          islands -= upper.get
          islands += Island(dot :: upper.get.dots)
        } else {
          islands += Island(dot :: Nil)
        }
      }
    }
    islands.toList
  }

  val xys = Array(
    Array(0,0,1,0,0,0,0,1,0,0,0,0,0), // 0
    Array(0,0,0,0,0,0,0,1,1,1,0,0,0), // 1
    Array(0,1,1,0,1,0,0,0,0,0,0,0,0), // 2
    Array(0,1,0,0,1,1,0,0,1,0,1,0,0),
    Array(0,1,0,0,1,1,0,0,1,1,1,0,0),
    Array(0,0,0,0,0,0,0,0,0,0,1,0,0),
    Array(0,0,0,0,0,0,0,1,1,1,0,0,0),
    Array(0,0,0,0,0,0,0,1,1,0,0,0,0)  // 7
  )

  println(islands(xys).sortBy(_.dots.size).reverse.head)
}
