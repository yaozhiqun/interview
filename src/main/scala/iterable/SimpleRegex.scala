package iterable

object SimpleRegex extends App {

  case class Foo(n: String, s: String, x: String, latency: Int)
  def par(str: String): Unit = {
    val rr = """^(\d{2}) ([abc]) (\w+) (\d+)ms$""".r
    str match {
      case rr(n, s, x, l) => println(Foo(n, s, x, l.toInt))
      case _ => println("none")
    }
  }

  par("22 a xxx 123ms")
  par("22 f xxx 12ms")
  par("a a xxx 1ms")
  par("02 a xxx 0ms")

}
