package rocketlawyer

import scala.collection.mutable.ListBuffer

object FlightTickets extends App {

  sealed trait Pixel {
    def x: Int
    def y: Int
  }
  case class Seat(x: Int, y: Int, row: Int, col: Char, var available: Boolean = true) extends Pixel {
    override def toString: String = {
      s"S($row,$col,${if (available) "AVAILA" else "OCCUPI"})"
    }

    def rightAdjacentTo(that: Seat): Boolean = {
      that.x == this.x && that.y + 1 == this.y
    }

    def rightNextTo(that: Seat): Boolean = {
      that.x == this.x && that.col.toInt + 1 == this.col.toInt
    }

    def sameRow(that: Seat): Boolean = {
      that.x == this.x
    }
  }
  case class Split(x: Int, y: Int) extends Pixel
  case class Empty(x: Int, y: Int) extends Pixel

  case class Ticket(row: Int, col: Char)

  case class Flight() {

    private val cols = 'A' to 'Z'
    private val pixels = ListBuffer[Pixel]()

    def fromList(xs: List[List[Char]]): Flight = {
      for {
        x <- xs.indices
        y <- xs(x).indices
      } {
        pixels += (xs(x)(y) match {
          case 'S' => Seat(x, y, x + 1, cols(pixels.count(p => p.x == x && p.isInstanceOf[Seat])))
          case 'A' => Split(x, y)
          case 'E' => Empty(x, y)
          case other => throw new IllegalArgumentException(s"Unknown flag $other")
        })
      }
      this
    }

    def book(row: Int, col: Char): Option[Ticket] = {
      seats.find(seat => seat.row == row && seat.col == col && seat.available).map(seat => {
        seat.available = false
        Ticket(seat.row, seat.col)
      })
    }

    def book(n: Int): List[Ticket] = {
      findSeats(n).flatMap(seat => book(seat.row, seat.col))
    }

    private def findSeats(n: Int): List[Seat] = {
      val nearestFront = ListBuffer[Seat]()
      val sameRow = ListBuffer[Seat]()
      val nextWithAisle = ListBuffer[Seat]()
      val nextEachOther = ListBuffer[Seat]()
      var preSeat: Option[Seat] = Option.empty

      pixels foreach {
        case seat@Seat(x, y, _, _, true) =>
          if (nearestFront.length < n)
            nearestFront += seat

          if (sameRow.length < n) {
            if (!preSeat.exists(seat.sameRow))
              sameRow.clear()

            sameRow += seat
          }

          if (nextWithAisle.length < n) {
            if (!preSeat.exists(seat.rightNextTo))
              nextWithAisle.clear()

            nextWithAisle += seat
         }

          if (nextEachOther.length < n && preSeat.exists(seat.rightAdjacentTo)) {
            nextEachOther += seat
            if (nextEachOther.length == n) return nextEachOther.toList
          } else {
            nextEachOther.clear()
            nextEachOther += seat
          }

          preSeat = Some(seat)
        case _ =>
      }

      if (nextWithAisle.length == n)
        nextWithAisle.toList
      else if (sameRow.length == n)
        sameRow.toList
      else if (nearestFront.length == n)
        nearestFront.toList
      else
        Nil
    }

    private def seats: List[Seat] = {
      pixels.toList.filter(_.isInstanceOf[Seat])map(_.asInstanceOf[Seat])
    }

    override def toString: String = {
      pixels.foldLeft(("", 0)) { case ((s, row), pixel) =>
        pixel match {
          case seat@Seat(x, y, _, _, _) =>
            if (x == row)
              if (y == 0)
                (s"${s}$seat", row)
              else
                (s"${s} $seat", row)
            else
              (s"${s}\n$seat", row + 1)
          case Split(_, _) =>
            (s"$s\t\t", row)
          case Empty(_, _) =>
            (s"$s              ", row)
          case _ => throw new IllegalStateException("Unknown object")
        }
      }._1
    }
  }

  object Flight {
    def apply(xs: List[List[Char]]): Flight = {
      new Flight().fromList(xs)
    }
  }

  val flight = Flight(List(
    List('S', 'S', 'S', 'A', 'S', 'S', 'S'),
    List('S', 'S', 'S', 'A', 'S', 'S', 'S'),
    List('S', 'S', 'S', 'A', 'S', 'S', 'S'),
    List('S', 'E', 'E', 'A', 'E', 'E', 'S')
  ))

  println(flight)
  println(flight.book(10))
  println(flight)
}
