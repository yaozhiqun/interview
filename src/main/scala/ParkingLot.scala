sealed trait ParkingSpaceType
case object HandicappedSpace extends ParkingSpaceType
case object RegularSpace extends ParkingSpaceType
case object CompactSpace extends ParkingSpaceType

case class ParkingSpace(number: Int,
                        `type`: ParkingSpaceType,
                        distance: Int,
                        var vacant: Boolean = true)

sealed trait ParkingLotSign
case object Vacancy extends ParkingLotSign
case object NoVacancy extends ParkingLotSign

case class ParkingLot(spaces: Set[ParkingSpace]) {

  def sign: ParkingLotSign = {
    if (spaces.exists(_.vacant))
      Vacancy
    else
      NoVacancy
  }

  def occupy(number: Int): ParkingSpace = {
    val space = getSpace(number)
    if (space.vacant) {
      space.vacant = false
      space
    } else
      throw new IllegalStateException(s"Cannot take occupied parking space $number")
  }

  def vacate(number: Int): Unit = {
    val space = getSpace(number)
    if (!space.vacant)
      space.vacant = true
    else
      throw new IllegalStateException(s"Cannot vacate vacant parking space $number")
  }

  def getSpace(number: Int): ParkingSpace = {
    spaces.find(_.number == number).getOrElse(throw new IllegalArgumentException(s"Parking space not found $number"))
  }

  def findNearestSpace(car: Car): Option[ParkingSpace] = {
    val vacantSpaces = spaces.filter(_.vacant)
    val available = car match {
      case Car(_, _, true) => vacantSpaces.filter(_.`type` == HandicappedSpace)
      case Car(_, Compact, _) => vacantSpaces.filter(space => space.`type` == RegularSpace || space.`type` == CompactSpace)
      case Car(_, Regular, _) => vacantSpaces.filter(space => space.`type` == RegularSpace)
    }
    available.toList.sortBy(_.distance).headOption
  }
}

sealed trait CarType
case object Compact extends CarType
case object Regular extends CarType

case class Car(plateNumber: String, carType: CarType, hasAssociationCard: Boolean = false) {

  def enter(parkingLot: ParkingLot): Option[Parking] = {
    parkingLot.sign match {
      case Vacancy => Some(Parking(parkingLot, this))
      case NoVacancy => None
    }
  }
}

case class Parking(parkingLot: ParkingLot, car: Car, parkingSpace: Option[ParkingSpace] = None) {

  def park(parkingSpaceNumber: Int): Option[ParkingSpace] = {
    val parkingSpace = parkingLot.getSpace(parkingSpaceNumber)

    def accessible: Boolean = {
      parkingSpace.`type` match {
        case HandicappedSpace if car.hasAssociationCard => true
        case CompactSpace => car.carType == Compact
        case RegularSpace => true
        case _ => false
      }
    }

    if (accessible) {
      val parkingSpace = parkingLot.occupy(parkingSpaceNumber)
      this.copy(parkingSpace = Some(parkingSpace))
      Some(parkingSpace)
    } else {
      println(s"Unable to take the parking space $parkingSpaceNumber")
      None
    }
  }

  def unpark(): Unit = {
    parkingSpace match {
      case Some(space) =>
        println(s"Car ${car.plateNumber} is vacanting the space ${space.number}")
        parkingLot.vacate(space.number)
      case None => throw new UnsupportedOperationException("You don't have a parking space to unpark.")
    }
  }
}

object ParkingLot extends App {

  override def main(args: Array[String]) {
    val parkingLot = ParkingLot(Set(
      ParkingSpace(1, HandicappedSpace, 10),
      ParkingSpace(2, HandicappedSpace, 11),
      ParkingSpace(3, HandicappedSpace, 12),
      ParkingSpace(4, CompactSpace, 13),
      ParkingSpace(5, CompactSpace, 14),
      ParkingSpace(6, CompactSpace, 15),
      ParkingSpace(7, RegularSpace, 16),
      ParkingSpace(8, RegularSpace, 17)
    ))

    def park(car: Car): Parking = {
      val spaceOpt = for {
        packing <- car.enter(parkingLot)
        nearestSpace <- packing.parkingLot.findNearestSpace(car)
        space <- packing.park(nearestSpace.number)
      } yield {
        space
      }

      spaceOpt match {
        case Some(space) =>
          println(s"${car.plateNumber} parked in space ${space.number}")
        case None =>
          println(s"${car.plateNumber} could not find a parking space")
          println(s"Parking lot sign: ${parkingLot.sign}")
          println(s"Available spaces: ${parkingLot.spaces.filter(_.vacant).map(_.number).mkString(" ")}")
          println(s"Available spaces: ${parkingLot.spaces collect { case space if space.vacant => space.number } mkString " "}")
      }

      Parking(parkingLot, car, spaceOpt)
    }

    park(Car("a1", Compact))
    park(Car("b2", Compact))
    park(Car("c3", Regular))
    park(Car("d4", Compact, hasAssociationCard = true))
    park(Car("e5", Regular))
    park(Car("f6", Regular))
    val parker7 = park(Car("g7", Compact))
    park(Car("h8", Compact))
    parker7.unpark()
    park(Car("h8", Compact))
  }
}
