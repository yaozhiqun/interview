import scala.io.Source

def maxWordsLine(filename: String): String = {

  Source.fromFile("/Users/zyao/Code/yaozhiqun/interview/src/main/resources/" + filename).getLines().foldLeft(("", 0)) {
    case ((maxWordsLine, wordNum), line) =>
      val num = line.split(" ").length
      if (line.split(" ").length > wordNum)
        (line, num)
      else
        (maxWordsLine, wordNum)
  }._1
}

maxWordsLine("max-repeated-file.txt")