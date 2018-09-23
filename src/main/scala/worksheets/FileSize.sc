import java.io.File

def fileSize(file: File): Long = {

  if (file.isFile) {
    file.length()
  } else {
    file.listFiles().foldLeft(0L) { case (size, child) =>
      size + fileSize(child)
    }
  }
}

fileSize(new File("/Users/zyao/Code/CIUtility"))