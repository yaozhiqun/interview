package coinbase

import scala.collection.mutable.ListBuffer

object FileSystemSolution extends App {

  val fileSplit = "/"
  
  trait FileObject {
    def name: String
    def path: String
    override def toString: String = name
  }

  case class File(name: String, parent: Dir, content: String) extends FileObject {
    def path: String = parent.path + fileSplit + name
  }

  case class Dir(name: String, parent: Option[Dir] = None, objects: ListBuffer[FileObject] = ListBuffer[FileObject]()) extends FileObject {
    def path: String = {
      def rec(dir: Dir, dirs: List[Dir] = Nil): List[Dir] = {
        dir.parent.map(p => rec(p, dir :: dirs)).getOrElse(dir :: dirs)
      }
      rec(this) match {
        case _ :: Nil => fileSplit
        case dirs => dirs.mkString(fileSplit)
      }
    }

    def findSubDir(subDirname: String): Option[Dir] = {
      objects.filter(_.isInstanceOf[Dir]).find(_.name == subDirname).map(_.asInstanceOf[Dir])
    }

    def findFile(filename: String): Option[File] = {
      objects.filterNot(_.isInstanceOf[Dir]).find(_.name == filename).map(_.asInstanceOf[File])
    }
  }

  case class FS() {
    val root: Dir = Dir("")

    def mkdir(path: String): Dir = {
      def rec(names: List[String], dir: Dir): Dir = {
        names match {
          case head :: tail =>
            val newSubDir = dir.findSubDir(head).getOrElse {
              val subDir = Dir(name = head, parent = Some(dir))
              dir.objects += subDir
              subDir
            }
            rec(tail, newSubDir)
          case _ =>
            dir
        }
      }
      path.split(fileSplit).toList match {
        case "" :: tail => rec(tail, root)
        case _ => throw new IllegalArgumentException("path must be absolute")
      }
    }

    def writeFile(path: String, data: String): File = {
      val lastFileSplitIndex = path.lastIndexOf(fileSplit)
      val filename = path.substring(lastFileSplitIndex + 1)
      val dirname = path.substring(0, lastFileSplitIndex)
      val dir = mkdir(dirname)
      val file = File(filename, content = data, parent = dir)
      dir.objects += file
      file
    }

    def readFile(path: String): Option[String] = {
      val lastFileSplitIndex = path.lastIndexOf(fileSplit)
      val filename = path.substring(lastFileSplitIndex + 1)
      val dirPath = path.substring(0, lastFileSplitIndex)

      def rec(names: List[String], dir: Dir): Option[Dir] = {
        names match {
          case head :: tail =>
            dir.findSubDir(head) match {
              case None => None
              case Some(subDir) => rec(tail, subDir)
            }
          case _ => Some(dir)
        }
      }

      dirPath.split(fileSplit).toList match {
        case "" :: tail => rec(tail, root).flatMap(_.findFile(filename)).map(_.content)
        case _ => throw new IllegalArgumentException("path must be absolute")
      }
    }

    def ls(path: String): List[FileObject] = {
      val lastFileSplitIndex = path.lastIndexOf(fileSplit)
      val lastObjectName = path.substring(lastFileSplitIndex + 1)
      val dirPath = path.substring(0, lastFileSplitIndex)

      def rec(names: List[String], dir: Dir): Option[Dir] = {
        names match {
          case head :: tail =>
            dir.findSubDir(head) match {
              case None => None
              case Some(subDir) => rec(tail, subDir)
            }
          case _ => Some(dir)
        }
      }

      val lastDirOp = dirPath.split(fileSplit).toList match {
        case "" :: tail => rec(tail, root)
        case _ => throw new IllegalArgumentException("path must be absolute")
      }

      lastDirOp match {
        case Some(dir) => dir.findSubDir(lastObjectName) match {
          case Some(subDir) => subDir.objects.toList
          case None => dir.findFile(lastObjectName).map(List(_)).getOrElse(Nil)
        }
        case _ => Nil
      }
    }
  }

  val fs = FS()
  val dir1 = fs.mkdir("/foo")
  val dir2 = fs.mkdir("/foo/bar")
  val dir3 = fs.mkdir("/foo/bar/sar")
  val dir4 = fs.mkdir("/foo/kkk")
  println(dir1.path)
  println(dir2.path)
  println(dir3.path)
  println(fs.root.path)
  println(dir4.path)
  val file1 = fs.writeFile("/foo/bar/hhh.txt", "hhh")
  println(file1.path)
  println(fs.readFile("/foo/bar/hhh.txt"))
  println(fs.mkdir("/foo/bar/hhh.txt"))
  println(fs.writeFile("/foo/bar/h.txt", "h"))
  println(fs.readFile("/foo/bar/h.txt"))
  println(fs.writeFile("/hhh.txt", "xxx"))
  println(fs.readFile("/hhh.txt"))
  println("=====")
  println(fs.ls("/foo/bar/h.txt").mkString("\n"))
  println(fs.ls("/foo/bar").mkString("\n"))
}
