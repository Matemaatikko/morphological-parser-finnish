package morph_fin.rulings.nouns

import morph_fin.kotus_format.SubwordWithInflection
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Genitive, Morphemes, Singular}
import morph_fin.rulings.rules.{DeclensionRule, Gradation}

object AllDeclensionUtils {

  def generateAllDeclections(word: Word)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val cases = DeclensionUtils.generateDeclensions(word)
    val genitive = cases.find(_.morphemes.is(Singular, Genitive)).get //Unsafe
    val comparativeForms = ComparationUtils.generateComparativeInflections(genitive)
    val superlativeForms = ComparationUtils.generateSuperlativeInflections(genitive)

    val adverbs = AdverbUtils.generate(cases)

    val normalWithSuffixes = cases.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))
    val comparativeWithSuffixes = comparativeForms.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))
    val superlativeWithSuffixes = superlativeForms.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))

    normalWithSuffixes ++ comparativeWithSuffixes ++ superlativeWithSuffixes // ++ adverbs


  def generateDeclectionsForCompound(lemma: String, prefix: SubwordWithInflection, suffix: SubwordWithInflection)
                                    (using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val prefixWord = Word.from(prefix.subword, prefix.inflection.rule, prefix.inflection.gradationLetter)
    val suffixWord = Word.from(suffix.subword, suffix.inflection.rule, suffix.inflection.gradationLetter, true)

    (prefix.inflection.rule, suffix.inflection.rule) match {
      case (first, 99) if first < 99 => generateAllDeclections(prefixWord).map(_ *:+ suffix.subword)
      case (first, second) if first < 99 && second < 99 => resolveWhereSuffixIndeclinable(lemma, prefixWord, suffixWord)
      case _ => throw new Error("Compound inflection error")
    }

  private inline def resolveWhereSuffixIndeclinable(lemma: String, pword: Word, sword: Word)
                                                   (using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val prefixCases = DeclensionUtils.generateDeclensions(pword)
    val suffixCases = DeclensionUtils.generateDeclensions(sword)

    val genitive = prefixCases.find(_.morphemes.is(Singular, Genitive)).get //Unsafe
    val comparativeForms = ComparationUtils.generateComparativeInflections(genitive)
    val superlativeForms = ComparationUtils.generateSuperlativeInflections(genitive)

    def resolve(suffix: InflectedWord): InflectedWord =
      val end = suffixCases.find(a => suffix.morphemes.is(a.morphemes)).get //Unsafe
      val inflectedWord = end +:* suffix.word.toString
      InflectedWord(inflectedWord.word, suffix.morphemes, lemma, end.gradationOpt)

    val normal = prefixCases.map(resolve(_))
    val comparative = comparativeForms.map(resolve(_))
    val superlative = superlativeForms.map(resolve(_))

    val normalWithSuffixes = normal.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))
    val comparativeWithSuffixes = comparative.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))
    val superlativeWithSuffixes = superlative.flatMap(value => PossessiveSuffixUtils.addSuffixes(value))

    normalWithSuffixes ++ comparativeWithSuffixes ++ superlativeWithSuffixes
}
