def getPattern(str: String): String = {
  str.toArray.zipWithIndex.foldLeft("") { case (pattern, (char, index)) =>
    if (pattern.length == 0)
      char.toString
    else {
      if (pattern.charAt(index % pattern.length) == char)
        pattern
      else
        str.substring(0, index + 1)
    }
  }
}

getPattern("")
getPattern("ABCABCA")
getPattern("ABACABACABA")
getPattern("ABACDABACDABA")
getPattern("ABACDABACDABA1")
getPattern("ABABCABABCA")