import java.io.File
import java.util.concurrent.{ForkJoinPool, RecursiveTask}

object FileSizeForkJoin extends App {

  case class FileFinder(file: File) extends RecursiveTask[Long] {
    override def compute(): Long = {
      if (file.isFile)
        file.length()
      else {
        val children = file.listFiles()
        val filesSize = children.filter(_.isFile).map(_.length()).sum
        val folders = children.filter(_.isDirectory).map(FileFinder)
        val foldersSize = folders.map(_.fork().join()).sum
        filesSize + foldersSize
      }
    }
  }

  val start = System.nanoTime()
  val pool = new ForkJoinPool
  val size = pool.invoke(FileFinder(new File("/Users/yao/loyal3/ach-service")))
  val end = System.nanoTime()

  println(s"Total size: $size")
  println(s"Time taken: ${(end - start) / 1.0e9}")
}
