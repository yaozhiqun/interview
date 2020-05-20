package docusign

object ShortestDistanceSolution extends App {

  val doc = """a b c d e f g h i j k hh kk dd s ee ss a dd cc ssd  a d c dd dd e dd bb dd dd a ee c as cde ddfd ddd"""

  case class Word(w: String, i: Int)

  def shortestDistance(doc: String, w1: String, w2: String): Int = {
    doc.split(" ").zipWithIndex.foldLeft((Option.empty[Word], Integer.MAX_VALUE)) { case ((lastWord, shortest), (word, index)) =>
      if (word == w1 || word == w2)
        lastWord match {
          case Some(Word(w, i)) =>
            if ((w == w1 && word == w2) || (w == w2 && word == w1)) {
              val newShortest = if (shortest < (index - i)) shortest else index - i
              (Some(Word(word, index)), newShortest)
            } else {
              (Some(Word(word, index)), shortest)
            }
          case _ => (Some(Word(word, index)), shortest)
        }
      else
        (lastWord, shortest)
    }._2
  }

  println(shortestDistance(doc, "a", "e"))
}
