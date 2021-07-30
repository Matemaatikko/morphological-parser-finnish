package morph_fin.kotus_format

import morph_fin.rulings.nomines.{Gradation, Word}

object EntryToWord {

  val letterMap = Map(
    'A' -> 0, 'B' -> 1, 'C' -> 2, 'D' -> 3,
    'E' -> 4, 'F' -> 5, 'G' -> 6, 'H' -> 7,
    'I' -> 8, 'J' -> 9, 'K' -> 10, 'L' -> 11,
    'M' -> 12, 'N' -> 13
  )

  val gradationMap = Seq(
    "kk" -> "k",
    "pp" -> "p",
    "tt" -> "t",
    "k" -> "",
    "p" -> "v",
    "t" -> "d",
    "nk" -> "ng",
    "mp" -> "mm",
    "lt" -> "ll",
    "nt" -> "nn",
    "rt" -> "rr",
    "k" -> "j",
    "k" -> "v"
  )

  def apply(entry: Entry): Option[Word] =
    entry.bending match {
      case Some(bending) => Some(
        Word(
          entry.word.value,
          bending.rule,
          bending.gradationLetter.map(
            l => {
              val (strong, weak) = gradationMap(letterMap(l))
              Gradation(strong, weak)
            })
        )
      )
      case None => None
    }

}
