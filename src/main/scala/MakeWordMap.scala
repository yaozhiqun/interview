object MakeWordMap extends App {

  def words(sentence: String): Array[String] = sentence.split(" ")

  val sentences = List(
    "This is the first sentence.",
    "This is the second sentence.",
    "This is the third sentence.")

  val wordMap = sentences.foldLeft(Map[String, List[String]]()) { (map, sentence) =>
    sentence.split(" ").foldLeft(map) { (map2, word) =>
      map2 + (word -> (sentence :: map2.getOrElse(word, Nil)))
    }
  }

  val wordAppearanceMap = sentences.foldLeft(Map[String, Int]()) { (map, sentence) =>
    sentence.split(" ").foldLeft(map) { (map2, word) =>
      map2 + (word -> (map2.getOrElse(word, 0) + 1))
    }
  }

  wordMap.foreach { case (k, v) => println(k -> v.length) }
  wordAppearanceMap.foreach(println)
}
