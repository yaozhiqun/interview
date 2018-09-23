package flexport.reservation

trait Event {
  def location: String
  def pieces: Int
  def timestamp: Int

  def +(another: Event): List[Event]
}

case class Arrival(location: String, pieces: Int, timestamp: Int) extends Event {
  def +(previous: Event): List[Event] = {
     this :: previous :: Nil
  }
}

case class Departure(location: String, pieces: Int, timestamp: Int) extends Event {
  def +(previous: Event): List[Event] = {
    previous match {
      case Arrival(ds, p, _) if ds == location & p == pieces => Nil
      case _ => this :: previous :: Nil
    }
  }
}

object ScheduleComplete {

  def isComplete(source: String, destination: String, pieces: Int, events: List[Event]): Boolean = {

    if (events.exists(_.pieces != pieces)) false
    else {
      val scheduled = events.foldLeft(List[Event]()) { (l, event) =>
        l match {
          case  head :: tail => (event + head) ::: tail
          case  Nil => event :: l
        }
      }.reverse

      println("------------")
      scheduled.foreach(println)

      scheduled match {
        case Departure(ds, _, _) :: Arrival(as, _, _) :: Nil
          if ds == source & as == destination => true
        case _ => false
      }
    }

  }
}
