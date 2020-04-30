import java.io.File

object FileSizeRec extends App {

  def fileSize(file: File): Long = {
    if (file.exists()) {
      if (file.isFile)
        file.length()
      else
        file.listFiles().par.collect {
          case f if f.isDirectory => fileSize(f)
          case f => f.length()
        }.sum
    } else {
      -1
    }
  }

  val start = System.nanoTime()
  val size = fileSize(new File("/tmp"))
  val end = System.nanoTime()

  println(s"Total size: $size")
  println(s"Time taken: ${(end - start) / 1.0e9}")

//
//  def fileSize(path: String): Long = {
//    val file = new File(path)
//    if (file.exists()) {
//      if (file.isFile) {
//        file.length()
//      } else {
//        file.listFiles().foldLeft(0L) { (size, file) =>
//          if (file.isFile) {
//            size + file.length()
//          } else {
//            size + fileSize(file.getPath)
//          }
//        }
//      }
//    } else {
//      -1L
//    }
//  }
}
