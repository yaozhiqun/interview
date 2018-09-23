def toMap(sentences: List[String]): Map[String, Int] = {

  sentences.flatMap(_.split(" ")).foldLeft(Map[String, Int]()) {
    case (map, word) =>
      map + (word -> (map.getOrElse(word, 0) + 1))
  }
}

val sentences = List(
  "hello credit karma",
  "hello world"
)

toMap(sentences)

def maxRepeatedWord(sentences: List[String]): Option[(String, Int)] = {
  toMap(sentences).toList.sortBy(_._2).reverse.headOption
}

maxRepeatedWord(sentences)