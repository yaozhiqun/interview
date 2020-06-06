package rocketlawyer

import org.scalatest.{FreeSpec, Matchers}
import rocketlawyer.FlightTickets.{Flight, Ticket}

class FlightTicketsSpec extends FreeSpec with Matchers {

  val ll = List(
    List('S', 'S', 'S', 'A', 'S', 'S', 'S'),
    List('S', 'S', 'S', 'A', 'S', 'S', 'S'),
    List('S', 'S', 'S', 'A', 'S', 'S', 'S'),
    List('S', 'E', 'E', 'A', 'E', 'E', 'S')
  )

  "FlightTickets" - {
    "Book 3 tickets should return 3 tickets" in {
      val flight = Flight(ll)
      flight.book(3) shouldBe List(Ticket(1, 'A'), Ticket(1, 'B'), Ticket(1, 'C'))
    }

    "Book 3 tickets should return 3 tickets next to each other" in {
      val flight = Flight(ll)
      // println(flight)
      flight.book(1, 'A')
      flight.book(1, 'B')
      // println(flight)
      flight.book(3) shouldBe List(Ticket(1, 'D'), Ticket(1, 'E'), Ticket(1, 'F'))
    }

    "Book 3 tickets should return 3 tickets next to each other on second row" in {
      val flight = Flight(ll)
      // println(flight)
      flight.book(1, 'A')
      flight.book(1, 'B')
      flight.book(1, 'F')
      // println(flight)
      flight.book(3) shouldBe List(Ticket(2, 'A'), Ticket(2, 'B'), Ticket(2, 'C'))
    }

    "Book 4 tickets should return 4 tickets next to each other split by aisle" in {
      val flight = Flight(ll)
      flight.book(1, 'A')
      flight.book(1, 'B')
      flight.book(4) shouldBe List(Ticket(1, 'C'), Ticket(1, 'D'), Ticket(1, 'E'), Ticket(1, 'F'))
    }

    "Book 3 tickets should return empty list if there are not enough available seats" in {
      val flight = Flight(ll)
      flight.book(1, 'A')
      flight.book(1, 'B')
      flight.book(1, 'C')
      flight.book(1, 'D')
      flight.book(1, 'E')
      flight.book(1, 'F')
      flight.book(2, 'A')
      flight.book(2, 'B')
      flight.book(2, 'C')
      flight.book(2, 'D')
      flight.book(2, 'E')
      flight.book(2, 'F')

      flight.book(8) shouldBe List(Ticket(3, 'A'), Ticket(3, 'B'), Ticket(3, 'C'), Ticket(3, 'D'), Ticket(3, 'E'), Ticket(3, 'F'), Ticket(4, 'A'), Ticket(4, 'B'))
      //println(flight)
      flight.book(1) shouldBe Nil // No more seat available
    }
  }
}
