import scala.collection.mutable.ListBuffer

object Playground extends App {

  sealed trait Pixel { def x: Int; def y: Int }
  case class Seat(x: Int, y: Int, row: Int, col: Char, var available: Boolean = true) extends Pixel {
    def sameRow(that: Seat): Boolean = this.row == that.row
    def nextTo(that: Seat): Boolean = sameRow(that) && (this.col.toInt == that.col.toInt + 1)
    def adjacentTo(that: Seat): Boolean = nextTo(that) && this.y == that.y + 1
  }
  case class Aisle(x: Int, y: Int) extends Pixel

  case class Flight(private val pixels: ListBuffer[Pixel]) {

    def book(row: Int, col: Int): Flight = {
       seats.find(s => s.row == row && s.col == col && s.available) foreach { seat =>
         seat.available = false
       }
      this
    }

    private def seats: List[Seat] = {
      pixels.filter(_.isInstanceOf[Seat]).map(_.asInstanceOf[Seat]).toList
    }

    def findSeats(n: Int): List[Seat] = {
      val nextEachOther = ListBuffer[Seat]()
      val nextWithAisle = ListBuffer[Seat]()
      val sameRow = ListBuffer[Seat]()
      val nearFront = ListBuffer[Seat]()
      var prevSeat = Option.empty[Seat]

      pixels.foreach {
        case seat: Seat if seat.available =>
          if (nearFront.length < n) {
            nearFront += seat
          }

          if (sameRow.length < n) {
            if (prevSeat.isDefined && seat.sameRow(prevSeat.get)) {
              sameRow += seat
            } else {
              sameRow.clear()
              sameRow += seat
            }
          }

          if (nextWithAisle.length < n) {
            if (prevSeat.isDefined && seat.nextTo(prevSeat.get)) {
              nextWithAisle += seat
            } else {
              nextWithAisle.clear()
              nextWithAisle += seat
            }
          }

          if (nextEachOther.length < n) {
            if (prevSeat.isDefined && seat.adjacentTo(prevSeat.get)) {
              nextEachOther += seat
              if (nextEachOther.length == n)
                return nextEachOther.toList
            } else {
              nextEachOther.clear()
              nextEachOther += seat
            }
          }
          prevSeat = Some(seat)
        case _ => // do nothing
      }

      if (nextWithAisle.length == n) {
        nextWithAisle.toList
      } else if (sameRow.length == n) {
        sameRow.toList
      } else if (nearFront.length == n) {
        nearFront.toList
      } else {
        Nil
      }
    }
  }

  object Flight {
    def apply(css: List[List[Char]]): Flight = {
      val cols = 'A' to 'Z'
      val pixels = ListBuffer[Pixel]()
      for { x <- css.indices; y <- css(x).indices } {
        css(x)(y) match {
          case 'S' =>
            val row = x + 1
            val col = cols(pixels.count(p => p.isInstanceOf[Seat] && p.x == x))
            pixels += Seat(x, y, row, col)
          case 'A' => pixels += Aisle(x, y)
        }
      }
      new Flight(pixels)
    }
  }

  val flight = Flight(List(
    List('S', 'S', 'S', 'A', 'S', 'S', 'S'),
    List('S', 'S', 'S', 'A', 'S', 'S', 'S')
  ))

  flight.findSeats(3).foreach(s => flight.book(s.row, s.col))
  println(flight)
  flight
    .book(1, 'D')
    .book(2, 'A')
  println(flight.findSeats(3))

}
