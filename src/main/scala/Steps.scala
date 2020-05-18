object Steps extends App {

  def steps(n: Int): String = {

    def recursive(row: Int = 0, col: Int = 0, s: String = ""): String = {

      if (row < n) {
        if (col < n) {
          if (col <= row)
            recursive(row, col + 1, s + "*")
          else
            recursive(row, col + 1, s + " ")
        } else {
          recursive(row + 1, 0, s + "\n")
        }
      } else {
        s
      }
    }

    recursive()

  }

  println(steps(8))

}
