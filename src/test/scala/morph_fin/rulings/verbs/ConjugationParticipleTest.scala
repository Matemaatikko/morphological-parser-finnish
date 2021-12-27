package morph_fin.rulings.verbs

import morph_fin.rulings.nouns.{DeclensionUtils, InflectedWord, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.morpheme.*
import morph_fin.rulings.rules.{ConjugationRule, DeclensionRule, LoadAndParseNounRules, LoadAndParseVerbRules}

class ConjugationParticipleTest extends AnyFlatSpec with should.Matchers {


  extension[A] (a: A)
    def opt = Some(a)
    
  import ConjugationUtils._
  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules
  given Seq[DeclensionRule] = LoadAndParseNounRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes.is(morphemes)).map(_.word.toString).toSet == words.toSet, morphemes)
    def matches_core(morphemes: Morphemes, words: String*) =
      val result = list.filter(a => a.morphemes.is(morphemes) && a.morphemes.isNotPossessiveSuffix && a.morphemes.isNotComparativeForm).map(_.word.toString).toSet
      assert(result == words.toSet, morphemes)

  "conjugationUtils" should "generete correctly for vAParticiple (Active) of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    //Singular
    conjugations.matches_core(vAParticiple ~ Active ~ Nominative ~ Singular, "painiva")
    conjugations.matches_core(vAParticiple ~ Active ~ Genitive ~ Singular, "painivan")
    conjugations.matches_core(vAParticiple ~ Active ~ Partitive ~ Singular, "painivaa")

    conjugations.matches_core(vAParticiple ~ Active ~ Illative ~ Singular, "painivaan")

    conjugations.matches_core(vAParticiple ~ Active  ~ Adessive ~ Singular, "painivalla")

    //Plural

    conjugations.matches_core(vAParticiple ~ Active ~ Nominative ~ Plural, "painivat")
    conjugations.matches_core(vAParticiple ~ Active ~ Genitive ~ Plural, "painivien", "painivain")
    conjugations.matches_core(vAParticiple ~ Active ~ Partitive ~ Plural, "painivia")

    conjugations.matches_core(vAParticiple ~ Active ~ Illative ~ Plural, "painiviin")

    conjugations.matches_core(vAParticiple ~ Active ~ Adessive ~ Plural, "painivilla")

    conjugations.matches_core(vAParticiple ~ Active ~ Instructive ~ Plural, "painivin")
    conjugations.matches_core(vAParticiple ~ Active ~ Comitative ~ Plural, "painivine")

    conjugations.matches(vAParticiple ~ Active ~ Nominative ~ Singular ~ Comparative, "painivampi")
    conjugations.matches(vAParticiple ~ Active ~ Nominative ~ Singular ~ Superlative, "painivin", "painivoin")
  }

  "conjugationUtils" should "generete correctly for vAParticiple (Passive) of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    //Singular
    conjugations.matches_core(vAParticiple ~ Passive ~ Nominative ~ Singular, "painittava")
    conjugations.matches_core(vAParticiple ~ Passive ~ Genitive ~ Singular, "painittavan")
    conjugations.matches_core(vAParticiple ~ Passive ~ Partitive ~ Singular, "painittavaa")

    conjugations.matches_core(vAParticiple ~ Passive ~ Illative ~ Singular, "painittavaan")

    conjugations.matches_core(vAParticiple ~ Passive  ~ Adessive ~ Singular, "painittavalla")

    //Plural

    conjugations.matches_core(vAParticiple ~ Passive ~ Nominative ~ Plural, "painittavat")
    conjugations.matches_core(vAParticiple ~ Passive ~ Genitive ~ Plural, "painittavien", "painittavain")
    conjugations.matches_core(vAParticiple ~ Passive ~ Partitive ~ Plural, "painittavia")

    conjugations.matches_core(vAParticiple ~ Passive ~ Illative ~ Plural, "painittaviin")

    conjugations.matches_core(vAParticiple ~ Passive ~ Adessive ~ Plural, "painittavilla")

    conjugations.matches_core(vAParticiple ~ Passive ~ Instructive ~ Plural, "painittavin")
    conjugations.matches_core(vAParticiple ~ Passive ~ Comitative ~ Plural, "painittavine")

    conjugations.matches(vAParticiple ~ Passive ~ Nominative ~ Singular ~ Comparative, "painittavampi")
    conjugations.matches(vAParticiple ~ Passive ~ Nominative ~ Singular ~ Superlative, "painittavin", "painittavoin")
  }

  "conjugationUtils" should "generete correctly for nUtParticiple of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    //Singular
    conjugations.matches_core(nUtParticiple ~ Nominative ~ Singular, "paininut")
    conjugations.matches_core(nUtParticiple ~ Genitive ~ Singular, "painineen")
    conjugations.matches_core(nUtParticiple ~ Partitive ~ Singular, "paininutta")

    conjugations.matches_core(nUtParticiple ~ Illative ~ Singular, "painineeseen")

    conjugations.matches_core(nUtParticiple  ~ Adessive ~ Singular, "painineella")

    //Plural

    conjugations.matches_core(nUtParticiple ~ Nominative ~ Plural, "painineet")
    conjugations.matches_core(nUtParticiple ~ Genitive ~ Plural, "painineiden", "painineitten")
    conjugations.matches_core(nUtParticiple ~ Partitive ~ Plural, "painineita")

    conjugations.matches_core(nUtParticiple ~ Illative ~ Plural, "painineisiin", "painineihin")

    conjugations.matches_core(nUtParticiple ~ Adessive ~ Plural, "painineilla")

    conjugations.matches_core(nUtParticiple ~ Instructive ~ Plural, "paininein")
    conjugations.matches_core(nUtParticiple ~ Comitative ~ Plural, "painineine")

    conjugations.matches(nUtParticiple ~ Nominative ~ Singular ~ Comparative, "painineempi")
    conjugations.matches(nUtParticiple ~ Nominative ~ Singular ~ Superlative, "paininein")
  }


  "conjugationUtils" should "generete correctly for tUParticiple of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    //Singular
    conjugations.matches_core(tUParticiple ~ Nominative ~ Singular, "painittu")
    conjugations.matches_core(tUParticiple ~ Genitive ~ Singular, "painitun")
    conjugations.matches_core(tUParticiple ~ Partitive ~ Singular, "painittua")

    conjugations.matches_core(tUParticiple ~ Illative ~ Singular, "painittuun")

    conjugations.matches_core(tUParticiple  ~ Adessive ~ Singular, "painitulla")

    //Plural

    conjugations.matches_core(tUParticiple ~ Nominative ~ Plural, "painitut")
    conjugations.matches_core(tUParticiple ~ Genitive ~ Plural, "painittujen")
    conjugations.matches_core(tUParticiple ~ Partitive ~ Plural, "painittuja")

    conjugations.matches_core(tUParticiple ~ Illative ~ Plural, "painittuihin")

    conjugations.matches_core(tUParticiple ~ Adessive ~ Plural, "painituilla")

    conjugations.matches_core(tUParticiple ~ Instructive ~ Plural, "painituin")
    conjugations.matches_core(tUParticiple ~ Comitative ~ Plural, "painittuine")

    conjugations.matches(tUParticiple ~ Nominative ~ Singular ~ Comparative, "painitumpi")
    conjugations.matches(tUParticiple ~ Nominative ~ Singular ~ Superlative, "painituin")
  }

  "conjugationUtils" should "generete correctly for AgentParticiple of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    //Singular
    conjugations.matches_core(AgentParticiple ~ Nominative ~ Singular, "painima")
    conjugations.matches_core(AgentParticiple ~ Genitive ~ Singular, "painiman")
    conjugations.matches_core(AgentParticiple ~ Partitive ~ Singular, "painimaa")

    conjugations.matches_core(AgentParticiple ~ Illative ~ Singular, "painimaan")

    conjugations.matches_core(AgentParticiple  ~ Adessive ~ Singular, "painimalla")

    //Plural

    conjugations.matches_core(AgentParticiple ~ Nominative ~ Plural, "painimat")
    conjugations.matches_core(AgentParticiple ~ Genitive ~ Plural, "painimien", "painimain")
    conjugations.matches_core(AgentParticiple ~ Partitive ~ Plural, "painimia")

    conjugations.matches_core(AgentParticiple ~ Illative ~ Plural, "painimiin")

    conjugations.matches_core(AgentParticiple ~ Adessive ~ Plural, "painimilla")

    conjugations.matches_core(AgentParticiple ~ Instructive ~ Plural, "painimin")
    conjugations.matches_core(AgentParticiple ~ Comitative ~ Plural, "painimine")

    conjugations.matches(AgentParticiple ~ Nominative ~ Singular ~ Comparative, "painimampi")
    conjugations.matches(AgentParticiple ~ Nominative ~ Singular ~ Superlative, "painimin", "painimoin")
  }


  "conjugationUtils" should "generete correctly for NegativeParticiple of word: painia" in {
    val word = Word("painia", 61, None)
    val conjugations = AllConjugationUtils.generateAllConjugations(word)

    //Singular
    conjugations.matches_core(NegativeParticiple ~ Nominative ~ Singular, "painimaton")
    conjugations.matches_core(NegativeParticiple ~ Genitive ~ Singular, "painimattoman")
    conjugations.matches_core(NegativeParticiple ~ Partitive ~ Singular, "painimatonta")

    conjugations.matches_core(NegativeParticiple ~ Illative ~ Singular, "painimattomaan")

    conjugations.matches_core(NegativeParticiple  ~ Adessive ~ Singular, "painimattomalla")

    //Plural

    conjugations.matches_core(NegativeParticiple ~ Nominative ~ Plural, "painimattomat")
    conjugations.matches_core(NegativeParticiple ~ Genitive ~ Plural, "painimattomien", "painimatonten")
    conjugations.matches_core(NegativeParticiple ~ Partitive ~ Plural, "painimattomia")

    conjugations.matches_core(NegativeParticiple ~ Illative ~ Plural, "painimattomiin")

    conjugations.matches_core(NegativeParticiple ~ Adessive ~ Plural, "painimattomilla")

    conjugations.matches_core(NegativeParticiple ~ Instructive ~ Plural, "painimattomin")
    conjugations.matches_core(NegativeParticiple ~ Comitative ~ Plural, "painimattomine")

    conjugations.matches(NegativeParticiple ~ Nominative ~ Singular ~ Comparative, "painimattomampi")
    conjugations.matches(NegativeParticiple ~ Nominative ~ Singular ~ Superlative, "painimattomin", "painimattomoin")
  }
}
