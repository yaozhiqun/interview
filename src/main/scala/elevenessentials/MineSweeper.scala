package elevenessentials

import scala.collection.mutable

object MineSweeper extends App {

  case class Dot(x: Int, y: Int) {
    def distance(that: Dot): Int = {
      if (that.x == x)
        Math.abs(that.y - y)
      else if (that.y == y)
        Math.abs(that.x - x)
      else
        Math.max(Math.abs(that.x - x), Math.abs(that.y - y))
    }
  }

  def solve(mines: List[(Int, Int)], rows: Int, cols: Int): Array[Array[Int]] = {
    val ms = mines.map(m => Dot(m._1, m._2))
    val aa = Array.ofDim[Int](3,4)

    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      val dot = Dot(row, col)
      if (ms.exists(_.distance(dot) == 0)) {
        aa(row)(col) = -1
      } else {
        aa(row)(col) = ms.count(_.distance(dot) == 1)
      }
    }
    aa.map(_.toArray).toArray
  }

  println(solve(List((0,0), (0, 1)), 3, 4).deep)
}
