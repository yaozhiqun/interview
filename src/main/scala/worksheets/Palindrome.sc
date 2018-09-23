def isPal(str: String): Boolean = {
  str.toArray.toList match {
    case head +: middle :+ last =>
      if (head == last)
        isPal(middle.mkString)
      else
        false
    case _ =>
      true
  }
}

isPal("abdcba")

def findAllPalindromeAnagrams(str: String): List[String] = {
  str.toArray
    .permutations
    .map(_.mkString)
    .toList
    .filter(isPal)
}

findAllPalindromeAnagrams("geeksgk").mkString("\n")

def hasPalAnagram(str: String): Boolean = {
  val charMap = str.toArray.foldLeft(Map[Char, Int]()) {
    case (map, char) =>
      map + (char -> (map.getOrElse(char, 0) + 1))
  }

  charMap.values.count(num => (num % 2) != 0) <= 1
}

hasPalAnagram("geeksforgeeks")
hasPalAnagram("geeksgeeks")

def findFirstPalAnagram(str: String): Option[String] = {
  if (hasPalAnagram(str)) {
    findAllPalindromeAnagrams(str).headOption
  } else {
    None
  }
}

findFirstPalAnagram("geeksgeeks")
findFirstPalAnagram("geeksforgeeks")