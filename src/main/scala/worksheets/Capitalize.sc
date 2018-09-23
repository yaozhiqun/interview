def capitalize(str: String): String = {
  str.toArray.foldLeft("") { (capitalized, char) =>
    capitalized + char.toUpper
  }
}

capitalize("Hello world")