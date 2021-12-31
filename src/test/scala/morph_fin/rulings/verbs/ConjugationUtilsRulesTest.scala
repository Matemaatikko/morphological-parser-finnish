package morph_fin.rulings.verbs

import morph_fin.rulings.nouns.{DeclensionUtils, InflectedWord, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.morpheme.*
import morph_fin.rulings.rules.{ConjugationRule, Gradation, LoadAndParseVerbRules}


class ConjugationUtilsRulesTest extends AnyFlatSpec with should.Matchers {

  extension[A] (a: A)
    def opt = Some(a)

  import ConjugationUtils._
  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes.is(morphemes)).map(_.word.toString).toSet == words.toSet, morphemes)

  "generateConjugations" should "handle class 52 with word: painua " in {
    val word = Word("painua", 52, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "painun")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "painutaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "painu")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "painuta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "painuit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "painuttiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "painunut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "painuneet")
    conjugations.matches(Finite ~ General ~ Passive, "painuttu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "painunee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "painuttaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "painune")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "painuttane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "painuisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "painuttaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "painuisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "painuttaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "painukaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "painuttakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "painu")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "painuko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "painuttako")

    // Nominal forms

    conjugations.matches(AInfinitive, "painua")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "painumalla")
    conjugations.matches(InfinitiveV, "painumaisilla")
    conjugations.matches(InfinitiveVI, "painuvina")
    conjugations.matches(InfinitiveVII, "painuttavissa")
    conjugations.matches(vAParticiple ~ Active, "painuva")
    conjugations.matches(NegativeParticiple, "painumaton")
  }

  "generateConjugations" should "handle class 53 with word: pakkauttaa " in {
    val word = Word("pakkauttaa", 53, Some(Gradation("tt", "t")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "pakkautan")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "pakkautetaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "pakkauta")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "pakkauteta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "pakkautit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "pakkautettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "pakkauttanut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "pakkauttaneet")
    conjugations.matches(Finite ~ General ~ Passive, "pakkautettu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "pakkauttanee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "pakkautettaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "pakkauttane")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "pakkautettane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "pakkauttaisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "pakkautettaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "pakkauttaisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "pakkautettaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "pakkauttakaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "pakkautettakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "pakkauta")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "pakkauttako")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "pakkautettako")

    // Nominal forms

    conjugations.matches(AInfinitive, "pakkauttaa")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "pakkauttamalla")
    conjugations.matches(InfinitiveV, "pakkauttamaisilla")
    conjugations.matches(InfinitiveVI, "pakkauttavina")
    conjugations.matches(InfinitiveVII, "pakkautettavissa")
    conjugations.matches(vAParticiple ~ Active, "pakkauttava")
    conjugations.matches(NegativeParticiple, "pakkauttamaton")
  }

  "generateConjugations" should "handle class 54 with word: parantaa " in {
    val word = Word("parantaa", 54, Some(Gradation("nt", "nn")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "parannan")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "parannetaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "paranna")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "paranneta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "paransit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "parannettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "parantanut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "parantaneet")
    conjugations.matches(Finite ~ General ~ Passive, "parannettu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "parantanee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "parannettaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "parantane")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "parannettane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "parantaisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "parannettaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "parantaisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "parannettaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "parantakaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "parannettakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "paranna")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "parantako")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "parannettako")

    // Nominal forms

    conjugations.matches(AInfinitive, "parantaa")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "parantamalla")
    conjugations.matches(InfinitiveV, "parantamaisilla")
    conjugations.matches(InfinitiveVI, "parantavina")
    conjugations.matches(InfinitiveVII, "parannettavissa")
    conjugations.matches(vAParticiple ~ Active, "parantava")
    conjugations.matches(NegativeParticiple, "parantamaton")
  }


  "generateConjugations" should "handle class 55 with word: kiitää " in {
    val word = Word("kiitää", 55, Some(Gradation("t", "d")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "kiidän")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "kiidetään")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "kiidä")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "kiidetä")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "kiisit", "kiidit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "kiidettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "kiitänyt")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "kiitäneet")
    conjugations.matches(Finite ~ General ~ Passive, "kiidetty")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "kiitänee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "kiidettäneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "kiitäne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "kiidettäne")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "kiitäisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "kiidettäisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "kiitäisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "kiidettäisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "kiitäkää")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "kiidettäköön")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "kiidä")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "kiitäkö")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "kiidettäkö")

    // Nominal forms

    conjugations.matches(AInfinitive, "kiitää")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "kiitämällä")
    conjugations.matches(InfinitiveV, "kiitämäisillä")
    conjugations.matches(InfinitiveVI, "kiitävinä")
    conjugations.matches(InfinitiveVII, "kiidettävissä")
    conjugations.matches(vAParticiple ~ Active, "kiitävä")
    conjugations.matches(NegativeParticiple, "kiitämätön")
  }

  "generateConjugations" should "handle class 56 with word: laulaa " in {
    val word = Word("laulaa", 56, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "laulan")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "lauletaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "laula")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "lauleta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "lauloit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "laulettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "laulanut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "laulaneet")
    conjugations.matches(Finite ~ General ~ Passive, "laulettu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "laulanee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "laulettaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "laulane")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "laulettane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "laulaisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "laulettaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "laulaisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "laulettaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "laulakaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "laulettakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "laula")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "laulako")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "laulettako")

    // Nominal forms

    conjugations.matches(AInfinitive, "laulaa")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "laulamalla")
    conjugations.matches(InfinitiveV, "laulamaisilla")
    conjugations.matches(InfinitiveVI, "laulavina")
    conjugations.matches(InfinitiveVII, "laulettavissa")
    conjugations.matches(vAParticiple ~ Active, "laulava")
    conjugations.matches(NegativeParticiple, "laulamaton")
  }

  "generateConjugations" should "handle class 57 with word: kaartaa " in {
    val word = Word("kaartaa", 57, Some(Gradation("rt", "rr")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "kaarran")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "kaarretaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "kaarra")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "kaarreta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "kaarroit", "kaarsit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "kaarrettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "kaartanut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "kaartaneet")
    conjugations.matches(Finite ~ General ~ Passive, "kaarrettu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "kaartanee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "kaarrettaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "kaartane")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "kaarrettane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "kaartaisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "kaarrettaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "kaartaisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "kaarrettaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "kaartakaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "kaarrettakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "kaarra")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "kaartako")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "kaarrettako")

    // Nominal forms

    conjugations.matches(AInfinitive, "kaartaa")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "kaartamalla")
    conjugations.matches(InfinitiveV, "kaartamaisilla")
    conjugations.matches(InfinitiveVI, "kaartavina")
    conjugations.matches(InfinitiveVII, "kaarrettavissa")
    conjugations.matches(vAParticiple ~ Active, "kaartava")
    conjugations.matches(NegativeParticiple, "kaartamaton")
  }

}
