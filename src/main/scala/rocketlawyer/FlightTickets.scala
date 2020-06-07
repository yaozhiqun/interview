package rocketlawyer

import scala.collection.mutable.ListBuffer

object FlightTickets extends App {

  sealed trait Pixel {
    def x: Int
    def y: Int
  }
  case class Seat(x: Int, y: Int, row: Int, col: Char, var booked: Boolean = false) extends Pixel {
    override def toString: String = {
      s"S($row,$col,${if (booked) "BOOKED" else "AVAILA"})"
    }
  }
  case class Empty(x: Int, y: Int) extends Pixel
  case class Aisle(x: Int, y: Int) extends Pixel

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
          case 'A' => Aisle(x, y)
          case 'E' => Empty(x, y)
          case other => throw new IllegalArgumentException(s"Unknown flag $other")
        })
      }
      this
    }

    def book(row: Int, col: Char): Option[Ticket] = {
      seats.find(seat => seat.row == row && seat.col == col && !seat.booked).map(seat => {
        seat.booked = true
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
      var splitByAisle = false

      pixels foreach {
        case seat@Seat(x, y, _, _, false) =>
          if (nearestFront.length < n)
            nearestFront += seat

          if (sameRow.length < n && preSeat.map(_.x).getOrElse(x) == x)
            sameRow += seat
          else {
            sameRow.clear()
            sameRow += seat
          }

          if (nextWithAisle.length < n && preSeat.map(_.x).getOrElse(x) == x && preSeat.map(_.y).getOrElse(y - 1) == y - 1)
            nextWithAisle += seat
          else {
            nextWithAisle.clear()
            nextWithAisle += seat
          }

          if (nextEachOther.length < n && preSeat.map(_.x).getOrElse(x) == x && preSeat.map(_.y).getOrElse(y - 1) == y - 1 && !splitByAisle) {
            nextEachOther += seat
            if (nextEachOther.length == n) return nextEachOther.toList
          } else {
            nextEachOther.clear()
            nextEachOther += seat
          }
          preSeat = Some(seat)
        case Seat(_, _, _, _, true) =>
        case Aisle(x, y) =>
          if (preSeat.getOrElse(x) == x) splitByAisle = true
        case _: Empty =>
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
          case Aisle(_, _) =>
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
