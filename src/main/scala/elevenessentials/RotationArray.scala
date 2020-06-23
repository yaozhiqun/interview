package elevenessentials

object RotationArray extends App {

  def isRotation(a: Array[Int], b: Array[Int]): Boolean = {
    if (a.length != b.length)
      false
    else {
      val jStart = b.indexOf(a.head)
      if (jStart == -1) return false
      a.indices foreach { i =>
        val j = (i + jStart) % b.length
        if (a(i) != b(j))
          return false
      }
      true
    }
  }

  println(isRotation(Array(1,2,3,4,5,6,7), Array(4,5,6,7,0,2,3)))
  println(isRotation(Array(1,2,3,4,5,6,7), Array(4,5,6,7,1,2,3)))
  println(isRotation(Array(2,3,4,5,6,7,1), Array(4,5,6,7,1,2,3)))
  println(isRotation(Array(2,3,4,5,6,1,7), Array(4,5,6,7,1,2,3)))
}
