import org.scalatest.{FreeSpec, Matchers}

class StepsSpec extends FreeSpec with Matchers {

  "Steps" - {

    "3 steps" in {
      Steps.steps(3)
    }

    "8 steps" in {
      Steps.steps(8)
    }
  }
}
