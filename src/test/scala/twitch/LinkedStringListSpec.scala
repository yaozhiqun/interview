package twitch

import org.scalatest.FlatSpec

class LinkedStringListSpec extends FlatSpec {

  val sentence = "This is a debug message"

  "A Solution" should "return welcome message" in {
    val result = LinkedStringList.fromSentence(sentence)

    assert(result === Some(Node("This",
      Some(Node("is",
        Some(Node("a",
          Some(Node("debug",
            Some(Node("message")))))))))))
  }

  "A Solution" should "return nth from last node" in {
    val result = LinkedStringList.fromSentence(sentence)

    assert(result.get.nthFromLast(5) === Some(Node("This",
      Some(Node("is",
        Some(Node("a",
          Some(Node("debug",
            Some(Node("message")))))))))))


    assert(result.get.nthFromLast(4) === Some(Node("is",
      Some(Node("a",
        Some(Node("debug",
          Some(Node("message")))))))))

    assert(result.get.nthFromLast(1) === Some(Node("message")))


    assert(result.get.nthFromLast(6) === None)
  }


  "A Solution" should "reverse welcome message" in {
    val result = LinkedStringList.fromSentence(sentence).map(_.reverse)

    assert(result === Some(Node("message",
      Some(Node("debug",
        Some(Node("a",
          Some(Node("is",
            Some(Node("This")))))))))))
  }

}
