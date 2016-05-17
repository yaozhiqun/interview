import scala.io.Source

object WordsInFile extends App {

  def words(filename: String): Set[String] = {
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(filename)).getLines()
      .flatMap(_.split(" ").filterNot(_.trim == ""))
      .foldLeft(Set[String]()) { case (words, word) =>
        words + word
      }
    }

  println(words("max-repeated-file.txt"))
}
