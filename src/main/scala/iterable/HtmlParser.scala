package iterable

import scala.io.Source

object HtmlParser extends App {

  val x1 = "<html><div><div></div></div><a href='#' />foobar</html>"
  val x2 = "<html></html>"

  def resolve(str: String): Map[String, Int] = {
    val regex = """(<(\w+)>|<(\w+)(.*)/>)""".r

    regex.findAllMatchIn(str).foldLeft(Map[String, Int]()) {
      case (m, regex(_, s1, s2, _)) =>
        val s = Option(s1).getOrElse(s2)
        m + (s -> m.get(s).map(_ + 1).getOrElse(1))
    }
  }

  resolve(x1).foreach(println)
  resolve(x2).foreach(println)

  def httpGet(uri: String): String = {
    Source.fromURL(uri)("ISO-8859-1").mkString
  }

  resolve(httpGet("https://www.google.com")).foreach(println)
}
