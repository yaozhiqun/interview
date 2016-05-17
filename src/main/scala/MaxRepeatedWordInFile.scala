import scala.io.Source

object MaxRepeatedWordInFile extends App {

  def findMaxRepeatedWordInFile(filename: String): Option[(String, Int)] = {
    val fileInputStream = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(filename))
    val wordsMap = fileInputStream.getLines()
      .flatMap(_.split(" ").filterNot(_.trim == ""))
      .foldLeft(Map[String, Int]()) { case (map, word) =>
        map + (word -> (map.getOrElse(word, 0) + 1))
      }
    wordsMap.toList.sortWith(_._2 > _._2).headOption
  }

  findMaxRepeatedWordInFile("max-repeated-file.txt").foreach(println)
}
