object MakeWordMap extends App {

  def words(sentence: String): Array[String] = sentence.split(" ")

  val sentences = List(
    "This is the first sentence.",
    "This is the second sentence.",
    "This is the third sentence.")

  val wordMap = sentences.flatMap(_.split(" ")).foldLeft(Map[String, Int]())
  { (map, word) =>
    map + (word -> (map.getOrElse(word, 0) + 1))
  }

  println(wordMap)

}
