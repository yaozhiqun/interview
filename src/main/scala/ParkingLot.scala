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
    if (spaces.exists(_.vacant == true))
      Vacancy
    else
      NoVacancy
  }

  def take(number: Int): ParkingSpace = {
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

    def findHandicappedSpaces = if (car.hasAssociationCard) {
      vacantSpaces.filter(_.`type` == HandicappedSpace).toList.sortBy(_.distance).headOption
    } else {
      None
    }

    def findOtherSpaces = {
      val available = car.carType match {
        case Compact => vacantSpaces.filter(space => space.`type` == RegularSpace || space.`type` == CompactSpace)
        case Regular => vacantSpaces.filter(space => space.`type` == RegularSpace)
      }
      available.toList.sortBy(_.distance).headOption
    }

    findHandicappedSpaces orElse findOtherSpaces
  }
}

sealed trait CarType
case object Compact extends CarType
case object Regular extends CarType

case class Car(plateNumber: String, carType: CarType, hasAssociationCard: Boolean = false) {

  def enter(parkingLot: ParkingLot): Option[Parker] = {
    parkingLot.sign match {
      case Vacancy => Some(Parker(parkingLot, this))
      case NoVacancy => None
    }
  }
}

case class Parker(parkingLot: ParkingLot, car: Car, parkingSpace: Option[ParkingSpace] = None) {

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
      val parkingSpace = parkingLot.take(parkingSpaceNumber)
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

    def park(car: Car): Parker = {
      val spaceOpt = for {
        parker <- car.enter(parkingLot)
        nearestSpace <- parker.parkingLot.findNearestSpace(car)
        space <- parker.park(nearestSpace.number)
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
      }

      Parker(parkingLot, car, spaceOpt)
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
