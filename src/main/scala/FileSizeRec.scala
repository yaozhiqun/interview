import java.io.File

object FileSizeRec extends App {

  def fileSize(file: File): Long = {
    file.listFiles.par.map { f =>
      if (f.isDirectory)
        fileSize(f)
      else
        f.length()
    }.sum
  }

  val start = System.nanoTime()
  val size = fileSize(new File("/Users/yao/loyal3/ach-service/"))
  val end = System.nanoTime()

  println(s"Total size: $size")
  println(s"Time taken: ${(end - start) / 1.0e9}")
}
