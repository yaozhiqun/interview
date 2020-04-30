import scala.io.Source

object MaxWordsLineInFile extends App {

  def findMaxWordCountLine(filename: String): String = {

    val lines = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(filename)).getLines().toList
    val maxWordCountLineIndex = lines.zipWithIndex.foldLeft(Map[Int, Int]()) { case (map, (line, index)) =>
        val wordCount = line.split(" ").filterNot(_.trim == "").length
        map + (index -> wordCount)
    }.toList.sortWith(_._2 > _._2).head._1

    lines(maxWordCountLineIndex)
  }

  println(findMaxWordCountLine("max-repeated-file.txt"))
}
