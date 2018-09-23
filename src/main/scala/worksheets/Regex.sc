val scala = "Scala is scalable and cool"

val pattern1 = "(S|s)cala".r

pattern1.findFirstIn(scala)

pattern1.findAllIn(scala).mkString(",")

val pattern2 = "Scala".r

pattern2.findFirstIn(scala)
pattern2.findAllIn(scala).mkString(",")

scala.matches(pattern1.toString())

pattern1.replaceAllIn(scala, "golang")

"abc".matches("[a-zA-Z]*")
"abc23".matches("[a-zA-Z0-9]*")