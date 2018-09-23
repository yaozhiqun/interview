// get_string("") => [""]
// get_strings("1") => ["a", "b"]
// get_strings("12") => ["ac", "ad", "bc", "bd"]
// get_strings("122") => ["acc", "acd", "adc", "add", "bcc", "bcd", "bdc", "bdd"]

val numberMap: Map[String, List[String]] = Map(
  "1" -> List("a", "b"),
  "2" -> List("c", "d"),
  "3" -> List("e", "f")
)

def getStrings(strs: List[String]): List[String] = {

  strs match {
    case head :: tail =>
      numberMap.getOrElse(head, List()).flatMap(alpha =>
        getStrings(tail).map(remainder => alpha + remainder))
      case _ =>
        List("")
  }
}

getStrings(List("1", "2", "3"))
