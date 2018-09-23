import java.io.File
import java.util.concurrent.{ForkJoinPool, RecursiveTask}

object FileSizeForkJoin extends App {

  case class FileFinder(file: File) extends RecursiveTask[Long] {
    override def compute(): Long = {
      file match {
        case ff if ff.exists() =>
          if (file.isFile)
            file.length()
          else {
            file.listFiles().collect {
              case f if f.isFile => f.length()
              case d if d.isDirectory => FileFinder(d).fork().join()
            }.sum
          }
        case _ => 0
      }
    }
  }

  val start = System.nanoTime()
  val pool = new ForkJoinPool
  val size = pool.invoke(FileFinder(new File("/tmp")))
  val end = System.nanoTime()

  println(s"Total size: $size")
  println(s"Time taken: ${(end - start) / 1.0e9}")
}
