package iterable

import org.joda.time.DateTime
import org.quartz.CronExpression

import scala.collection.mutable.ListBuffer
import scala.util.Random

object EventSchedulerSolution extends App {

  case class Participant(name: String, var accepted: Option[Boolean] = None)

  trait ScheduleType
  case object Meeting extends ScheduleType
  case object Holiday extends ScheduleType
  case object Birthday extends ScheduleType
  case object Reminder extends ScheduleType

  trait Schedule {
    def id: Int
    def locationOpt: Option[String]
    def inviter: Participant
    def invitees: List[Participant]
    def `type`: ScheduleType
    def participants: List[Participant] = {
      inviter :: invitees
    }
    def acceptedBy(name: String): Unit = {
      participants.find(_.name == name).foreach(_.accepted = Some(true))
    }
    def rejectedBy(name: String): Unit = {
      participants.find(_.name == name).foreach(_.accepted = Some(false))
    }
  }

  case class AcceptedSchedule(schedule: Schedule, participant: Participant)

  case class Event(id: Int, startAt: DateTime, endAt: DateTime, locationOpt: Option[String] = None, inviter: Participant, invitees: List[Participant], `type`: ScheduleType) extends Schedule

  case class Recurring(id: Int, cronExpression: CronExpression, minutes: Int, locationOpt: Option[String] = None, inviter: Participant, invitees: List[Participant], `type`: ScheduleType) extends Schedule {
    def toEvents(startAt: DateTime, endAt: DateTime): List[Event] = {

      def recur(startAt: DateTime, endAt: DateTime, events: List[Event] = Nil): List[Event] = {
        Option(cronExpression.getNextInvalidTimeAfter(startAt.toDate)) match {
          case Some(time) if !time.after(endAt.toDate) =>
            val eventStartAt = new DateTime(time)
            val eventEndAt = eventStartAt.plusMinutes(minutes)

            recur(eventStartAt, endAt, events :+ Event(Random.nextInt(100), eventStartAt, eventEndAt, locationOpt, inviter, invitees, `type`))
          case _ =>
            events
        }
      }

      recur(startAt, endAt)
    }
  }

  case class Scheduler() {
    private val events = ListBuffer[Event]()
    private val recurrings = ListBuffer[Recurring]()

    def schedule(schedule: Schedule): Scheduler = {
      schedule match {
        case e: Event => events += e
        case r: Recurring => recurrings += r
      }

      this
    }

    def listEvents(startAt: DateTime, endAt: DateTime): List[Event] = {
      require(endAt.isAfter(startAt), s"End time $endAt is supposed to be after start time $startAt")
      (events.filter(e => e.startAt.isAfter(startAt) && e.startAt.isBefore(endAt)) ++ recurrings.flatMap(_.toEvents(startAt, endAt)))
        .sortBy(_.startAt.getMillis).toList
    }

    def acceptEvent(name: String, eventId: Int): Scheduler = {
      events.find(_.id == eventId).foreach(_.acceptedBy(name))
      this
    }

    def acceptRecurring(name: String, recurringId: Int): Scheduler = {
      recurrings.find(_.id == recurringId).foreach(_.acceptedBy(name))
      this
    }

    def rejectEvent(name: String, eventId: Int): Scheduler = {
      events.find(_.id == eventId).foreach(_.rejectedBy(name))
      this
    }

    def rejectRecurring(name: String, recurringId: Int): Scheduler = {
      recurrings.find(_.id == recurringId).foreach(_.rejectedBy(name))
      this
    }
  }


  val now = new DateTime()
  Scheduler()
    .schedule(Event(1, now, now.plusMinutes(30), inviter = Participant("Zach"), invitees = List(Participant("Kevin"), Participant("Emen")), `type` = Meeting))
    .schedule(Event(2, now.plusMinutes(45), now.plusHours(1), inviter = Participant("Zach"), invitees = List(Participant("Jerry")), `type` = Reminder))
    .schedule(Recurring(3, new CronExpression("0 0/15 * 1/1 * ? *"), 5, inviter = Participant("Isa"), invitees = List(Participant("Zach")), `type` = Holiday))
    .acceptEvent("Emen", 1)
    .listEvents(now.minusMinutes(1), now.plusMinutes(40))
    .foreach(println)


}
