case class Storage(city: String, pieces: Int = 0)

sealed trait Journey {
  def city: String
  def pieces: Int
  def time: Int
}

case class Departure(city: String, pieces: Int, time: Int) extends Journey {
  def withdraw(storage: Storage): Storage = {
    storage.copy(pieces = storage.pieces - pieces)
  }
}

case class Arrival(city: String, pieces: Int, time: Int) extends Journey {
  def store(storage: Storage): Storage = {
    storage.copy(pieces = storage.pieces + pieces)
  }
}

def isCompleted(fromCity: String, toCity: String, pieces: Int, journeys: List[Journey]): Boolean = {
 journeys.sortBy(_.time) match {
   case Departure(departureCity, shipPieces, _) +: _ :+ Arrival(arriveCity, _, _) =>
     if (departureCity != fromCity || arriveCity != toCity) {
       false
     } else {
       journeys.foldLeft(Map[String, Storage](fromCity -> Storage(fromCity, pieces))) {
         case (map, arrival@Arrival(city, _, _)) =>
           map + (city -> arrival.store(map.getOrElse(city, Storage(city))))
         case (map, departure@Departure(city, _, _)) =>
           map.get(city) match {
             case Some(storage) => map + (city -> departure.withdraw(storage))
             case _ => return false
           }
       }.values.map(_.pieces).sum == shipPieces
     }
   case _ =>
     false
 }
}

isCompleted("hk", "lax", 10, List(
  Departure("hk", 10, 0),
  Arrival("lax", 10, 1))
)

isCompleted("hk", "lax", 10, List(
  Departure("hk", 10, 0),
  Arrival("anc", 10, 1),
  Departure("anc", 10, 2),
  Arrival("lax", 10, 3))
)

isCompleted("hk", "lax", 10, List(
  Departure("hk", 10, 0),
  Arrival("lax", 8, 1)
))

isCompleted("hk", "lax", 10, List(
  Departure("hk", 10, 0),
  Departure("anc", 10, 1),
  Arrival("anc", 10, 2),
  Arrival("lax", 10, 3))
)

isCompleted("hk", "lax", 10, List(
  Departure("anc", 10, 0),
  Arrival("lax", 10, 3)
))