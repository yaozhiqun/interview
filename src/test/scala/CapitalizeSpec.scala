import org.scalatest.{FreeSpec, Matchers}

class CapitalizeSpec extends FreeSpec with Matchers {

  "Capitalize" - {

    "test1" in {
      Capitalize.capitalize("hello world") shouldBe "Hello World"
     }

    "test2" in {
      Capitalize.capitalize("a hello world") shouldBe "A Hello World"
     }

    "test3" in {
      Capitalize.capitalize("hello!") shouldBe "Hello!"
     }
  }
}
