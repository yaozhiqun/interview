case class Table(id: Int, maxServe: Int)

case class Reservation(hour: Int, table: Table)

case class Restaurant(tables: Set[Table], reservations: List[Reservation] = Nil) {

}

val tables: Set[Table] = Set(
  Table(1, 2),
  Table(2, 2),
  Table(3, 3),
  Table(4, 3),
  Table(5, 4),
  Table(6, 4),
  Table(7, 10),
  Table(8, 10)
)

val rest0 = Restaurant(tables)

object Booking {

  def book(hour: Int, guests: Int, restaurant: Restaurant): (Option[Reservation], Restaurant) = {
    val reservationOpt = restaurant.tables
      .filter(_.maxServe >= guests).toList
      .sortBy(_.maxServe)
      .filterNot { table =>
        val reservedTables = restaurant.reservations.filter(_.hour == hour).map(_.table)
        reservedTables.map(_.id).contains(table.id)
      }.headOption
      .map(Reservation(hour, _))

    val bookedRestaurant: Restaurant = reservationOpt.map(reservation => restaurant.copy(reservations = reservation :: restaurant.reservations)).getOrElse(restaurant)
    (reservationOpt, bookedRestaurant)
  }
}

val (reser1, rest1) = Booking.book(19, 3, rest0)
val (reser2, rest2) = Booking.book(19, 3, rest1)
val (reser3, rest3) = Booking.book(19, 3, rest2)
val (reser4, rest4) = Booking.book(20, 3, rest3)
val (reser5, rest5) = Booking.book(20, 11, rest4)
