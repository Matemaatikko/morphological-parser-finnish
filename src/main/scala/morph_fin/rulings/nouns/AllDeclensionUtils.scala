package morph_fin.rulings.nouns

import morph_fin.rulings.*
import morph_fin.rulings.rules.{DeclensionRule, Gradation}

//TODO untested
object AllDeclensionUtils {

  def generateAllDeclections(word: Word)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val cases = DeclensionUtils.generateDeclensions(word)
    val genitive = cases.find(_.morphemes.is(Singular, Genitive)).get
    val comparativeForms = ComparationUtils.generateComparativeInflections(genitive)
    val superlativeForms = ComparationUtils.generateSuperlativeInflections(genitive)

    val adverbs = AdverbUtils.generate(genitive)

    val normalWithSuffixes = cases.flatMap(value => PossessiveSuffixUtils.addSuffixes(value, word.gradationOpt))
    val comparativeWithSuffixes = comparativeForms.flatMap(value => PossessiveSuffixUtils.addSuffixes(value, Some(Gradation("mp", "mm"))))
    val superlativeWithSuffixes = superlativeForms.flatMap(value => PossessiveSuffixUtils.addSuffixes(value, None))

    normalWithSuffixes ++ comparativeWithSuffixes ++ superlativeWithSuffixes ++ adverbs
}