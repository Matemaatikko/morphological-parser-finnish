package morph_fin.rulings.verbs

import morph_fin.kotus_format.UpdatedWord._
import morph_fin.kotus_format.{LoadUpdatedKotus, UpdatedWord}
import morph_fin.rulings.nouns.{AllDeclensionUtils, Filters, Word}
import morph_fin.rulings.rules.{ConjugationRule, DeclensionRule, LoadAndParseNounRules, LoadAndParseVerbRules}

class VerbPrinter {

  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules

  val words = LoadUpdatedKotus.apply()

  def print(lemma: String): String =
    val resultOpt = words.find(_.value == lemma)

    resultOpt match {
      case Some(StandardBending(lemma, inflection)) =>
        val word = Word.from(lemma, inflection.rule, inflection.gradationLetter)
        val conjugations = ??? //TODO generage
        //Resolve
        ???
      case _ => ""
    }

}
