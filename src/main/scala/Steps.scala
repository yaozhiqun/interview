object Steps {

  def steps(n: Int): Unit = {

    def recursive(row: Int, stair: Int, s: String): Unit = {

      if (row < n) {
        if (stair < n) {
          if (stair <= row)
            recursive(row, stair + 1, s + "*")
          else
            recursive(row, stair + 1, s + " ")
        } else {
          println(s)
          recursive(row + 1, 0, "")
        }
      }
    }

    recursive(0, 0, "")
  }

}
