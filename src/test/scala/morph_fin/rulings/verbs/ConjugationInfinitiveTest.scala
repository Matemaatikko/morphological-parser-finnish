package morph_fin.rulings.verbs

import morph_fin.rulings.nouns.{DeclensionUtils, InflectedWord, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.morpheme.*
import morph_fin.rulings.rules.{ConjugationRule, DeclensionRule, LoadAndParseNounRules, LoadAndParseVerbRules}



class ConjugationInfinitiveTest extends AnyFlatSpec with should.Matchers {

  extension[A] (a: A)
    def opt = Some(a)

  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules
  given Seq[DeclensionRule] = LoadAndParseNounRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes.is(morphemes)).map(_.word.toString).toSet == words.toSet, morphemes)
    def matches_core(morphemes: Morphemes, words: String*) =
      val result = list.filter(a => a.morphemes.is(morphemes) && a.morphemes.isNotPossessiveSuffix && a.morphemes.isNotComparativeForm).map(_.word.toString).toSet
      assert(result == words.toSet, morphemes)


  "possessivesuffixes" should "generete correctly for infinitive forms of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    // Present

    conjugations.matches(AInfinitiveLong ~ PossessiveSuffix.SingularFirst, "painiakseni")
    conjugations.matches(EInfinitive ~ Inessive ~ Active ~ PossessiveSuffix.PluralSecond, "painiessanne")
    conjugations.matches(InfinitiveV ~ PossessiveSuffix.ThirdPos, "painimaisillaan", "painimaisillansa")
    conjugations.matches(InfinitiveVI ~ PossessiveSuffix.SingularSecond, "painivinasi")

  }

  "conjugationUtils" should "generete correctly for IVth infinitive of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    //Singular
    conjugations.matches_core(InfinitiveIV ~ Nominative ~ Singular, "painiminen")
    conjugations.matches_core(InfinitiveIV ~ Genitive ~ Singular, "painimisen")
    conjugations.matches_core(InfinitiveIV ~ Partitive ~ Singular, "painimista")

    conjugations.matches_core(InfinitiveIV ~ Illative ~ Singular, "painimiseen")

    conjugations.matches_core(InfinitiveIV ~ Adessive ~ Singular, "painimisella")

    //Plural

    conjugations.matches_core(InfinitiveIV ~ Nominative ~ Plural, "painimiset")
    conjugations.matches_core(InfinitiveIV ~ Genitive ~ Plural, "painimisten", "painimisien")
    conjugations.matches_core(InfinitiveIV ~ Partitive ~ Plural, "painimisia")

    conjugations.matches_core(InfinitiveIV ~ Illative ~ Plural, "painimisiin")

    conjugations.matches_core(InfinitiveIV ~ Adessive ~ Plural, "painimisilla")

    conjugations.matches_core(InfinitiveIV ~ Instructive ~ Plural, "painimisin")
    conjugations.matches_core(InfinitiveIV ~ Comitative ~ Plural, "painimisine")

    conjugations.matches(InfinitiveIV ~ Nominative ~ Singular ~ Comparative, "painimisempi")
    conjugations.matches(InfinitiveIV ~ Nominative ~ Singular ~ Superlative, "painimisin")
  }

}
