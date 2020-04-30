package iterable

object SimpleRegex extends App {

  case class Foo(n: String, s: String, x: String)
  def par(str: String): Unit = {
    val rr = """^(\d{2}) (a|b|c) (.*)$""".r
    str match {
      case rr(n, s, x) => println(Foo(n, s, x))
      case _ => println("none")
    }
  }

  par("22 a xxx")
  par("22 f xxx")
  par("a a xxx")
  par("02 a xxx")

}
