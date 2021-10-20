package morph_fin.rulings.nouns

import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Genitive, Singular}
import morph_fin.rulings.rules.{DeclensionRule, Gradation}

object AllDeclensionUtils {

  def generateAllDeclections(word: Word)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val cases = DeclensionUtils.generateDeclensions(word)
    val genitive = cases.find(_.morphemes.is(Singular, Genitive)).get
    val comparativeForms = ComparationUtils.generateComparativeInflections(genitive)
    val superlativeForms = ComparationUtils.generateSuperlativeInflections(genitive)

    val adverbs = AdverbUtils.generate(cases)

    val normalWithSuffixes = cases.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))
    val comparativeWithSuffixes = comparativeForms.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))
    val superlativeWithSuffixes = superlativeForms.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))

    normalWithSuffixes ++ comparativeWithSuffixes ++ superlativeWithSuffixes // ++ adverbs
}
