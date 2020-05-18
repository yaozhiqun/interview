package iterable

import java.time.format.DateTimeFormatter
import java.util.UUID

import org.joda.time.{DateTime, format}
import org.joda.time.format.DateTimeFormat
import org.quartz.CronExpression

import scala.collection.mutable.ListBuffer

object EventSchedulerSolution extends App {

  case class Person(name: String) {
    def createEvent(`type`: ScheduleType, startAt: DateTime, durationInMin: Int, invitees: List[Participant], locationOpt: Option[String] = None): Event = {
      Event(startAt = startAt, durationInMin = durationInMin, locationOpt = locationOpt, `type` = `type`, participants = Participant(this, host = true, accepted = Some(true)) :: invitees)
    }

    def createRecurring(`type`: ScheduleType, cronExpression: CronExpression, durationInMin: Int, invitees: List[Participant], locationOpt: Option[String] = None): Recurring = {
      Recurring(cronExpression = cronExpression, durationInMin = durationInMin, locationOpt = locationOpt, `type` = `type`, participants = Participant(this, host = true, accepted = Some(true)) :: invitees)
    }

    private[iterable] def accept(schedule: Schedule): Unit = {
      schedule.participants.find(_.person == this).foreach(_.accepted = Some(true))
    }

    def reject(schedule: Schedule): Unit = {
      schedule.participants.find(_.person == this).foreach(_.accepted = Some(false))
    }
  }

  case class Participant(person: Person, host: Boolean = false, var accepted: Option[Boolean] = None) {
    override def toString: String = {
      s"${person.name}(${accepted.getOrElse("pending")})"
    }
  }

  trait ScheduleType
  case object Meeting extends ScheduleType
  case object TearBreak extends ScheduleType
  case object Birthday extends ScheduleType
  case object Reminder extends ScheduleType

  trait Schedule {
    def id: String
    def durationInMin: Int
    def locationOpt: Option[String]
    def `type`: ScheduleType
    def participants: List[Participant]
    def inviter: Participant = {
      participants.find(_.host).getOrElse(throw new IllegalStateException("Could not find host of this event"))
    }
    def invitees: List[Participant] = {
      participants.filter(!_.host)
    }
  }

  case class AcceptedSchedule(schedule: Schedule, participant: Participant)

  case class Event(id: String = UUID.randomUUID().toString,
                   startAt: DateTime,
                   durationInMin: Int,
                   locationOpt: Option[String] = None,
                   participants: List[Participant] = Nil,
                   `type`: ScheduleType) extends Schedule {
    def endAt: DateTime = startAt.plusMinutes(durationInMin)

    override def toString: String = {
      s"[$startAt - $endAt]($id) ${`type`} hosted by $inviter invitees: ${invitees.mkString(" ")}"
    }

    val dateTimeFormat: format.DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  }

  case class Recurring(id: String = UUID.randomUUID().toString,
                       cronExpression: CronExpression, durationInMin: Int,
                       locationOpt: Option[String] = None,
                       participants: List[Participant],
                       `type`: ScheduleType) extends Schedule {
    def toEvents(startAt: DateTime, endAt: DateTime): List[Event] = {

      def recur(startAt: DateTime, events: List[Event] = Nil): List[Event] = {
        Option(cronExpression.getNextValidTimeAfter(startAt.toDate)) match {
          case Some(time) if !time.after(endAt.toDate) =>
            val eventStartAt = new DateTime(time)
            val event = Event(startAt = eventStartAt, durationInMin = durationInMin, locationOpt = locationOpt, participants = participants, `type` = `type`)
            recur(eventStartAt, events :+ event)
          case _ =>
            events
        }
      }

      recur(startAt)
    }
  }

  case class Scheduler() {
    private val events = ListBuffer[Event]()
//    private val recurrings = ListBuffer[Recurring]()

    def schedule(schedule: Schedule): Scheduler = {
      schedule match {
        case e: Event => events += e
        case r: Recurring =>
          val now = new DateTime()
          events ++= r.toEvents(now, now.plusDays(1))
      }

      this
    }

    def listEvents(startAt: DateTime, endAt: DateTime): List[Event] = {
      require(endAt.isAfter(startAt), s"End time $endAt is supposed to be after start time $startAt")
      events.filter(e => e.startAt.isAfter(startAt) && e.startAt.isBefore(endAt))
        .sortBy(_.startAt.getMillis).toList
    }

    def available(person: Person, startAt: DateTime, endAt: DateTime): Boolean = {
      listEvents(new DateTime(), endAt).filter(_.participants.exists(p => p.person == person && p.accepted == Some(true))).forall(_.endAt.isBefore(startAt))
    }

    def accept(person: Person, event: Event): Unit = {
      if (available(person, event.startAt, event.endAt))
        person.accept(event)
      else
        println(s"Time conflicting for ${person.name} to join $event")
    }
  }

  val now = new DateTime()

  val zach = Person("Zach")
  val emen = Person("Emen")
  val jerry = Person("Jerry")
  val kevin = Person("Kevin")
  val isa = Person("Isa")

  val e1 = zach.createEvent(startAt = now, durationInMin = 30, invitees = List(Participant(kevin), Participant(emen), Participant(isa)), `type` = Meeting)
  val e2 = jerry.createEvent(startAt = now.plusMinutes(45), durationInMin = 60, invitees = List(Participant(zach)), `type` = Reminder)
  val r1 = isa.createRecurring(cronExpression = new CronExpression("0 0/15 * 1/1 * ? *"), durationInMin = 5, invitees = List(Participant(zach)), `type` = TearBreak)

  val scheduler = Scheduler()
    .schedule(e1)
    .schedule(e2)
    .schedule(r1)
  scheduler.listEvents(now.minusMinutes(1), now.plusMinutes(60)).foreach(println)

  println("-------------------------")
  scheduler.accept(kevin, e1)
  scheduler.accept(emen, e1)
  // accept a recurring that could impact accept other events
//  scheduler.listEvents(now.minusMinutes(1), now.plusMinutes(60)).find(_.invitees.exists(_.person == zach)).foreach(scheduler.accept(zach, _))
  scheduler.accept(zach, e2)

  scheduler.listEvents(now.minusMinutes(60), now.plusMinutes(120)).foreach(println)
}
