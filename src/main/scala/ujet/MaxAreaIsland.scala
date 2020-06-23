 package ujet

import scala.collection.mutable.ListBuffer

object MaxAreaIsland extends App {

  case class Dot(row: Int, col: Int) {
    def adjacentLeft(islands: List[Island]): Option[Island] = {
      islands.find(_.dots.exists(d => d.col + 1 == col && d.row == row))
    }

    def adjacentTop(islands: List[Island], dot: Dot): Option[Island] = {
      islands.find(_.dots.exists(d => d.col == dot.col && d.row + 1 == dot.row))
    }
  }

  case class Island(dots: List[Dot])

  def islands(xys: Array[Array[Int]]): List[Island] = {
    val islands = ListBuffer[Island]()
    for {
      row  <- xys.indices
      col <- xys(row).indices
    } {
      if (xys(row)(col) == 1) {
        val dot = Dot(row, col)
        val leftIsl = dot.adjacentLeft(islands.toList)
        val topIsl = dot.adjacentTop(islands.toList, dot)
        (leftIsl, topIsl) match {
          case (Some(left), Some(top)) =>
            islands -= left
            islands -= top
            islands += Island((dot :: left.dots ::: top.dots).distinct)
          case (Some(left), None) =>
            islands -= left
            islands += Island(dot :: left.dots)
          case (None, Some(top)) =>
            islands -= top
            islands += Island(dot :: top.dots)
          case _ =>
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
    Array(0,1,0,0,1,1,0,0,1,0,1,0,0), // 3
    Array(0,1,0,0,1,1,0,0,1,1,1,0,0), // 4
    Array(0,0,0,0,0,0,0,0,0,0,1,0,0), // 5
    Array(0,0,0,0,0,0,0,1,1,1,0,0,0), // 6
    Array(0,0,0,0,0,0,0,1,1,0,0,0,0)  // 7
  )

  val is = islands(xys)
//  islands(xys).foreach(println)
//  println(islands(xys).sortBy(_.dots.size).reverse.head)
  println(is.maxBy(_.dots.size).dots.size)
}
