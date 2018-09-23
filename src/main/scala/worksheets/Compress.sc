def compress(str: String): String = {
  str.foldLeft(List[(Char, Int)]()) { case (list, char) =>
    list match {
      case head :+ last =>
        if (last._1 == char)
          head :+ (char, last._2 + 1)
        else
          list :+ (char, 1)
      case _ =>
        List((char, 1))
    }
  }.map(e => e._1.toString + e._2).mkString
}

compress("1aaaaabcccddd")