package iterable

object SimpleRegex extends App {

<<<<<<< HEAD
  case class Foo(n: String, s: String, x: String)
  def par(str: String): Unit = {
    val rr = """^(\d{2}) (a|b|c) (.*)$""".r
    str match {
      case rr(n, s, x) => println(Foo(n, s, x))
=======
  case class Foo(n: String, s: String, x: String, latency: Int)
  def par(str: String): Unit = {
    val rr = """^(\d{2}) (a|b|c) (\w+) (\d+)ms$""".r
    str match {
      case rr(n, s, x, l) => println(Foo(n, s, x, l.toInt))
>>>>>>> 6d9129e02e9db8a6f3c34b6e6199bc494669ff8e
      case _ => println("none")
    }
  }

<<<<<<< HEAD
  par("22 a xxx")
  par("22 f xxx")
  par("a a xxx")
  par("02 a xxx")
=======
  par("22 a xxx 123ms")
  par("22 f xxx 12ms")
  par("a a xxx 1ms")
  par("02 a xxx 0ms")
>>>>>>> 6d9129e02e9db8a6f3c34b6e6199bc494669ff8e

}
