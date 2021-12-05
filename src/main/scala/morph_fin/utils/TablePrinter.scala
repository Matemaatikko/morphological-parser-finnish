package morph_fin.utils

object TablePrinter {
  //It is assumed that listOfRows is table.
  def printTable(listOfRows: Seq[Seq[String]]): String =
    val numberOfRows = listOfRows.length
    val numberOfColumns = listOfRows(0).length

    val maxLenghtsInColumns: Seq[Int] = for(i <- 0 until numberOfColumns) yield {
      var max = 0
      for(j <- 0 until numberOfRows) {
        val elem = listOfRows(j)(i)
        if(elem.length > max) max = elem.length
      }
      max
    }

    val lineSeparator = "-"*(maxLenghtsInColumns.sum + 3*numberOfColumns)

    (for(j <- 0 until numberOfRows) yield {
      val row = listOfRows(j)
      val filled = for(i <- 0 until numberOfColumns) yield {
        val elem = row(i)
        elem + " "*(maxLenghtsInColumns(i) - elem.length)
      }
      filled.mkString("| ", " | ", " |\n")
    }).mkString(lineSeparator+"\n", lineSeparator+"\n", lineSeparator)
  end printTable
}
