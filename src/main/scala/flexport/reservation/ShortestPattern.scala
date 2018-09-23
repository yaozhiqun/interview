package flexport.reservation

import scala.annotation.tailrec

object ShortestPattern {

  def findPattern(str: String): String = {

    @tailrec
    def recursive(pattern: String, tail: String): String = {
      (pattern, tail) match {
        case (_, "") => pattern
        case ("", t) => recursive(t.take(1), t.substring(1))
        case (p, t) if t.startsWith(p) | p.startsWith(t) => recursive(p, t.substring(Math.min(p.length, t.length)))
        case _ => recursive(str.take(pattern.length + 1), str)
      }
    }

    recursive("", str)
  }

}
