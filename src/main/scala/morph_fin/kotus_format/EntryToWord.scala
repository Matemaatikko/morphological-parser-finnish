package morph_fin.kotus_format

import morph_fin.rulings.GradationHandler
import morph_fin.rulings.nouns.Word

object EntryToWord {

  def apply(entry: Entry): Option[Word] =
    entry.inflectionOpt match {
      case Some(bending) =>
        Some(Word(
          entry.word.value,
          bending.rule,
          bending.gradationLetter.map(GradationHandler.getGradationByLetter(_))
        ))
      case None => None
    }

}
