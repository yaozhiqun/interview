package flexport.reservation

import org.scalatest.{FreeSpec, Matchers}

class ScheduleCompleteTest extends FreeSpec with Matchers {

  "#isComplete" - {

    "returns true for good schedules" in {
      ScheduleComplete.isComplete("hk", "lax", 10, List(
        Departure("hk", 10, 0),
        Arrival("lax", 10, 1)
      )) shouldBe true
    }

    "returns true for good schedules - with stopover" in {
      ScheduleComplete.isComplete("hk", "lax", 10, List(
        Departure("hk", 10, 0),
        Arrival("anc", 10, 1),
        Departure("anc", 10, 2),
        Arrival("lax", 10, 3)
      )) shouldBe true
    }

    "returns false for bad schedules" in {
      ScheduleComplete.isComplete("hk", "lax", 10, List(
        Departure("hk", 10, 0),
        Arrival("lax", 8, 1)
      )) shouldBe false
    }

    "returns false for bad schedules - depart before arrival" in {
      ScheduleComplete.isComplete("hk", "lax", 10, List(
        Departure("hk", 10, 0),
        Departure("anc", 10, 1),
        Arrival("anc", 10, 2),
        Arrival("lax", 10, 3)
      )) shouldBe false
    }

    "returns false for bad source location" in {
      ScheduleComplete.isComplete("hk", "lax", 10, List(
        Departure("anc", 10, 0),
        Arrival("lax", 10, 3)
      )) shouldBe false
    }
  }
}
