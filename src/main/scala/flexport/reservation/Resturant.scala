package flexport.reservation

import scala.collection.mutable.ListBuffer

case class Person(name: String, phone: String)

case class Table(id: Int, capacity: Int, reservations: ListBuffer[Reservation] = ListBuffer()) {

  override def toString: String = {
    s"Table $id seats $capacity currently has ${reservations.size} reservations"
  }
}

case class Reservation(reservedBy: Person, reserved: Table, reservedAt: Int)

case class Restaurant(tables: Set[Table], openAt: Int = 18, closeAt: Int = 23) {

  def reserve(person: Person, partyNum: Int, requestAt: Int): Option[Reservation] = {

    if (requestAt < openAt || requestAt >= closeAt) {
      println("Closed")
      None
    } else {
      val reservation = tables
        .filter(t => t.capacity == partyNum)
        .find { t =>
          !t.reservations.map(_.reservedAt).contains(requestAt)
        }
        .map(Reservation(person, _, requestAt))

      reservation match {
        case Some(r) => r.reserved.reservations += Reservation(r.reservedBy, r.reserved, r.reservedAt)
        case None => println("No table available at that moment")
      }

      reservation
    }
  }
}

object Test extends App {
  val jack = Person("Jack", "9343453223")
  val tables = Set(Table(1, 2), Table(2, 4), Table(3, 6))
  val restaurant = Restaurant(tables)
  restaurant.reserve(jack, 6, 10).foreach(println(_))
  restaurant.reserve(jack, 6, 18).foreach(println(_))
  restaurant.reserve(jack, 4, 19).foreach(println(_))
  restaurant.reserve(jack, 4, 20).foreach(println(_))
  restaurant.reserve(jack, 6, 18).foreach(println(_))
  restaurant.tables.map(_.reservations).filter(_.nonEmpty).flatten.foreach(println(_))
}


