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

  "generateConjugations" should "handle class 58 with word: tunkea " in {
    val word = Word("tunkea", 58, Some(Gradation("nk", "ng")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "tungen")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "tungetaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "tunge")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "tungeta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "tungit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "tungettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "tunkenut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "tunkeneet")
    conjugations.matches(Finite ~ General ~ Passive, "tungettu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "tunkenee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "tungettaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "tunkene")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "tungettane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "tunkisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "tungettaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "tunkisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "tungettaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "tunkekaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "tungettakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "tunge")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "tunkeko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "tungettako")

    // Nominal forms

    conjugations.matches(AInfinitive, "tunkea")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "tunkemalla")
    conjugations.matches(InfinitiveV, "tunkemaisilla")
    conjugations.matches(InfinitiveVI, "tunkevina")
    conjugations.matches(InfinitiveVII, "tungettavissa")
    conjugations.matches(vAParticiple ~ Active, "tunkeva")
    conjugations.matches(NegativeParticiple, "tunkematon")
  }

  //SKIP: 59, 60 (Contains only single word)


  "generateConjugations" should "handle class 61 with word: marssia " in {
    val word = Word("marssia", 61, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "marssin")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "marssitaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "marssi")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "marssita")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "marssit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "marssittiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "marssinut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "marssineet")
    conjugations.matches(Finite ~ General ~ Passive, "marssittu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "marssinee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "marssittaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "marssine")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "marssittane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "marssisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "marssittaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "marssisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "marssittaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "marssikaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "marssittakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "marssi")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "marssiko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "marssittako")

    // Nominal forms

    conjugations.matches(AInfinitive, "marssia")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "marssimalla")
    conjugations.matches(InfinitiveV, "marssimaisilla")
    conjugations.matches(InfinitiveVI, "marssivina")
    conjugations.matches(InfinitiveVII, "marssittavissa")
    conjugations.matches(vAParticiple ~ Active, "marssiva")
    conjugations.matches(NegativeParticiple, "marssimaton")
  }

  "generateConjugations" should "handle class 62 with word: meditoida " in {
    val word = Word("meditoida", 62, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "meditoin")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "meditoidaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "meditoi")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "meditoida")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "meditoit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "meditoitiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "meditoinut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "meditoineet")
    conjugations.matches(Finite ~ General ~ Passive, "meditoitu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "meditoinee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "meditoitaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "meditoine")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "meditoitane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "meditoisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "meditoitaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "meditoisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "meditoitaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "meditoikaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "meditoitakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "meditoi")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "meditoiko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "meditoitako")

    // Nominal forms

    conjugations.matches(AInfinitive, "meditoida")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "meditoimalla")
    conjugations.matches(InfinitiveV, "meditoimaisilla")
    conjugations.matches(InfinitiveVI, "meditoivina")
    conjugations.matches(InfinitiveVII, "meditoitavissa")
    conjugations.matches(vAParticiple ~ Active, "meditoiva")
    conjugations.matches(NegativeParticiple, "meditoimaton")
  }

  "generateConjugations" should "handle class 63 with word: myydä " in {
    val word = Word("myydä", 63, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "myyn")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "myydään")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "myy")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "myydä")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "myit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "myytiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "myynyt")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "myyneet")
    conjugations.matches(Finite ~ General ~ Passive, "myyty")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "myynee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "myytäneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "myyne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "myytäne")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "myisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "myytäisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "myisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "myytäisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "myykää")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "myytäköön")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "myy")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "myykö")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "myytäkö")

    // Nominal forms

    conjugations.matches(AInfinitive, "myydä")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "myymällä")
    conjugations.matches(InfinitiveV, "myymäisillä")
    conjugations.matches(InfinitiveVI, "myyvinä")
    conjugations.matches(InfinitiveVII, "myytävissä")
    conjugations.matches(vAParticiple ~ Active, "myyvä")
    conjugations.matches(NegativeParticiple, "myymätön")
  }

  "generateConjugations" should "handle class 64 with word: suoda " in {
    val word = Word("suoda", 64, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "suon")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "suodaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "suo")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "suoda")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "soit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "suotiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "suonut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "suoneet")
    conjugations.matches(Finite ~ General ~ Passive, "suotu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "suonee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "suotaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "suone")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "suotane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "soisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "suotaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "soisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "suotaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "suokaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "suotakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "suo")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "suoko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "suotako")

    // Nominal forms

    conjugations.matches(AInfinitive, "suoda")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "suomalla")
    conjugations.matches(InfinitiveV, "suomaisilla")
    conjugations.matches(InfinitiveVI, "suovina")
    conjugations.matches(InfinitiveVII, "suotavissa")
    conjugations.matches(vAParticiple ~ Active, "suova")
    conjugations.matches(NegativeParticiple, "suomaton")
  }

  // SKIP: 65 (single word)

  "generateConjugations" should "handle class 66 with word: lakaista " in {
    val word = Word("lakaista", 66, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "lakaisin")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "lakaistaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "lakaise")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "lakaista")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "lakaisit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "lakaistiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "lakaissut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "lakaisseet")
    conjugations.matches(Finite ~ General ~ Passive, "lakaistu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "lakaissee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "lakaistaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "lakaisse")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "lakaistane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "lakaisisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "lakaistaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "lakaisisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "lakaistaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "lakaiskaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "lakaistakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "lakaise")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "lakaisko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "lakaistako")

    // Nominal forms

    conjugations.matches(AInfinitive, "lakaista")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "lakaisemalla")
    conjugations.matches(InfinitiveV, "lakaisemaisilla")
    conjugations.matches(InfinitiveVI, "lakaisevina")
    conjugations.matches(InfinitiveVII, "lakaistavissa")
    conjugations.matches(vAParticiple ~ Active, "lakaiseva")
    conjugations.matches(NegativeParticiple, "lakaisematon")
  }


  "generateConjugations" should "handle class 67 with word: laulella " in {
    val word = Word("laulella", 67, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "laulelen")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "laulellaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "laulele")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "laulella")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "laulelit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "lauleltiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "laulellut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "laulelleet")
    conjugations.matches(Finite ~ General ~ Passive, "lauleltu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "laulellee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "lauleltaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "laulelle")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "lauleltane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "laulelisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "lauleltaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "laulelisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "lauleltaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "laulelkaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "lauleltakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "laulele")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "laulelko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "lauleltako")

    // Nominal forms

    conjugations.matches(AInfinitive, "laulella")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "laulelemalla")
    conjugations.matches(InfinitiveV, "laulelemaisilla")
    conjugations.matches(InfinitiveVI, "laulelevina")
    conjugations.matches(InfinitiveVII, "lauleltavissa")
    conjugations.matches(vAParticiple ~ Active, "lauleleva")
    conjugations.matches(NegativeParticiple, "laulelematon")
  }

  "generateConjugations" should "handle class 68 with word: luennoida " in {
    val word = Word("luennoida", 68, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "luennoin", "luennoitsen")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "luennoidaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "luennoi", "luennoitse")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "luennoida")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "luennoit", "luennoitsit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "luennoitiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "luennoinut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "luennoineet")
    conjugations.matches(Finite ~ General ~ Passive, "luennoitu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "luennoinee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "luennoitaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "luennoine")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "luennoitane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "luennoisimme", "luennoitsisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "luennoitaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "luennoisi", "luennoitsisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "luennoitaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "luennoikaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "luennoitakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "luennoi", "luennoitse")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "luennoiko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "luennoitako")

    // Nominal forms

    conjugations.matches(AInfinitive, "luennoida")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "luennoimalla", "luennoitsemalla")
    conjugations.matches(InfinitiveV, "luennoimaisilla", "luennoitsemaisilla")
    conjugations.matches(InfinitiveVI, "luennoivina", "luennoitsevina")
    conjugations.matches(InfinitiveVII, "luennoitavissa")
    conjugations.matches(vAParticiple ~ Active, "luennoiva", "luennoitseva")
    conjugations.matches(NegativeParticiple, "luennoimaton", "luennoitsematon")
  }


  "generateConjugations" should "handle class 69 with word: merkitä " in {
    val word = Word("merkitä", 69, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "merkitsen")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "merkitään")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "merkitse")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "merkitä")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "merkitsit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "merkittiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "merkinnyt")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "merkinneet")
    conjugations.matches(Finite ~ General ~ Passive, "merkitty")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "merkinnee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "merkittäneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "merkinne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "merkittäne")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "merkitsisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "merkittäisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "merkitsisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "merkittäisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "merkitkää")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "merkittäköön")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "merkitse")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "merkitkö")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "merkittäkö")

    // Nominal forms

    conjugations.matches(AInfinitive, "merkitä")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "merkitsemällä")
    conjugations.matches(InfinitiveV, "merkitsemäisillä")
    conjugations.matches(InfinitiveVI, "merkitsevinä")
    conjugations.matches(InfinitiveVII, "merkittävissä")
    conjugations.matches(vAParticiple ~ Active, "merkitsevä")
    conjugations.matches(NegativeParticiple, "merkitsemätön")
  }

  "generateConjugations" should "handle class 70 with word: syöstä " in {
    val word = Word("syöstä", 70, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "syöksen")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "syöstään")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "syökse")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "syöstä")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "syöksit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "syöstiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "syössyt")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "syösseet")
    conjugations.matches(Finite ~ General ~ Passive, "syösty")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "syössee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "syöstäneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "syösse")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "syöstäne")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "syöksisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "syöstäisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "syöksisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "syöstäisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "syöskää")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "syöstäköön")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "syökse")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "syöskö")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "syöstäkö")

    // Nominal forms

    conjugations.matches(AInfinitive, "syöstä")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "syöksemällä")
    conjugations.matches(InfinitiveV, "syöksemäisillä")
    conjugations.matches(InfinitiveVI, "syöksevinä")
    conjugations.matches(InfinitiveVII, "syöstävissä")
    conjugations.matches(vAParticiple ~ Active, "syöksevä")
    conjugations.matches(NegativeParticiple, "syöksemätön")
  }

  "generateConjugations" should "handle class 71 with word: tehdä " in {
    val word = Word("tehdä", 71, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "teen")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "tehdään")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "tee")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "tehdä")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "teit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "tehtiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "tehnyt")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "tehneet")
    conjugations.matches(Finite ~ General ~ Passive, "tehty")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "tehnee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "tehtäneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "tehne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "tehtäne")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "tekisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "tehtäisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "tekisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "tehtäisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "tehkää")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "tehtäköön")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "tee")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "tehkö")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "tehtäkö")

    // Nominal forms

    conjugations.matches(AInfinitive, "tehdä")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "tekemällä")
    conjugations.matches(InfinitiveV, "tekemäisillä")
    conjugations.matches(InfinitiveVI, "tekevinä")
    conjugations.matches(InfinitiveVII, "tehtävissä")
    conjugations.matches(vAParticiple ~ Active, "tekevä")
    conjugations.matches(NegativeParticiple, "tekemätön")
  }

  "generateConjugations" should "handle class 72 with word: paeta " in {
    val word = Word("paeta", 72, Some(Gradation("k", "")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "pakenen")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "paetaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "pakene")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "paeta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "pakenit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "paettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "paennut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "paenneet")
    conjugations.matches(Finite ~ General ~ Passive, "paettu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "paennee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "paettaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "paenne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "paettane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "pakenisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "paettaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "pakenisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "paettaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "paetkaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "paettakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "pakene")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "paetko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "paettako")

    // Nominal forms

    conjugations.matches(AInfinitive, "paeta")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "pakenemalla")
    conjugations.matches(InfinitiveV, "pakenemaisilla")
    conjugations.matches(InfinitiveVI, "pakenevina")
    conjugations.matches(InfinitiveVII, "paettavissa")
    conjugations.matches(vAParticiple ~ Active, "pakeneva")
    conjugations.matches(NegativeParticiple, "pakenematon")
  }

  "generateConjugations" should "handle class 73 with word: paikata " in {
    val word = Word("paikata", 73, Some(Gradation("kk", "k")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "paikkaan")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "paikataan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "paikkaa")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "paikata")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "paikkasit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "paikattiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "paikannut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "paikanneet")
    conjugations.matches(Finite ~ General ~ Passive, "paikattu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "paikannee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "paikattaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "paikanne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "paikattane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "paikkaisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "paikattaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "paikkaisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "paikattaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "paikatkaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "paikattakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "paikkaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "paikatko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "paikattako")

    // Nominal forms

    conjugations.matches(AInfinitive, "paikata")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "paikkaamalla")
    conjugations.matches(InfinitiveV, "paikkaamaisilla")
    conjugations.matches(InfinitiveVI, "paikkaavina")
    conjugations.matches(InfinitiveVII, "paikattavissa")
    conjugations.matches(vAParticiple ~ Active, "paikkaava")
    conjugations.matches(NegativeParticiple, "paikkaamaton")
  }


  "generateConjugations" should "handle class 74 with word: pinota " in {
    val word = Word("pinota", 74, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "pinoan")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "pinotaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "pinoa")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "pinota")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "pinosit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "pinottiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "pinonnut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "pinonneet")
    conjugations.matches(Finite ~ General ~ Passive, "pinottu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "pinonnee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "pinottaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "pinonne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "pinottane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "pinoisimme", "pinoaisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "pinottaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "pinoisi", "pinoaisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "pinottaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "pinotkaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "pinottakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "pinoa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "pinotko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "pinottako")

    // Nominal forms

    conjugations.matches(AInfinitive, "pinota")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "pinoamalla")
    conjugations.matches(InfinitiveV, "pinoamaisilla")
    conjugations.matches(InfinitiveVI, "pinoavina")
    conjugations.matches(InfinitiveVII, "pinottavissa")
    conjugations.matches(vAParticiple ~ Active, "pinoava")
    conjugations.matches(NegativeParticiple, "pinoamaton")
  }

  "generateConjugations" should "handle class 75 with word: selvitä " in {
    val word = Word("selvitä", 75, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "selviän")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "selvitään")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "selviä")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "selvitä")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "selvisit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "selvittiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "selvinnyt")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "selvinneet")
    conjugations.matches(Finite ~ General ~ Passive, "selvitty")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "selvinnee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "selvittäneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "selvinne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "selvittäne")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "selviäisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "selvittäisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "selviäisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "selvittäisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "selvitkää")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "selvittäköön")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "selviä")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "selvitkö")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "selvittäkö")

    // Nominal forms

    conjugations.matches(AInfinitive, "selvitä")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "selviämällä")
    conjugations.matches(InfinitiveV, "selviämäisillä")
    conjugations.matches(InfinitiveVI, "selviävinä")
    conjugations.matches(InfinitiveVII, "selvittävissä")
    conjugations.matches(vAParticiple ~ Active, "selviävä")
    conjugations.matches(NegativeParticiple, "selviämätön")
  }

  "generateConjugations" should "handle class 76 with word: tietää " in {
    val word = Word("tietää", 76, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "tiedän")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "tiedetään")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "tiedä")
    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "tiedetä")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "tiesit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "tiedettiin", "tiettiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "tietänyt", "tiennyt")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "tietäneet", "tienneet")
    conjugations.matches(Finite ~ General ~ Passive, "tiedetty", "tietty")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "tietänee", "tiennee")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "tiedettäneen", "tiettäneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "tietäne", "tienne")
    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "tiedettäne", "tiettäne")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "tietäisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "tiedettäisiin", "tiettäisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "tietäisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "tiedettäisi", "tiettäisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "tietäkää")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "tiedettäköön", "tiettäköön")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "tiedä")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "tietäkö")
    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "tiedettäkö", "tiettäkö")

    // Nominal forms

    conjugations.matches(AInfinitive, "tietää")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "tietämällä")
    conjugations.matches(InfinitiveV, "tietämäisillä")
    conjugations.matches(InfinitiveVI, "tietävinä")
    conjugations.matches(InfinitiveVII, "tiedettävissä")
    conjugations.matches(vAParticiple ~ Active, "tietävä")
    conjugations.matches(NegativeParticiple, "tietämätön")
  }

  "generateConjugations" should "handle class 77 with word: heläjää " in {
    val word = Word("heläjää", 77, None)
    val conjugations = generateConjugations(word)

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Positive, "heläjää")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Negative, "heläjä")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularThird ~ Positive, "heläji")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Positive, "heläjäisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Negative, "heläjäisi")

    conjugations.matches(AInfinitive, "heläjää")
    conjugations.matches(vAParticiple ~ Active, "heläjävä")
  }

  "generateConjugations" should "handle class 78 with word: kilkkaa " in {
    val word = Word("kilkkaa", 78, None)
    val conjugations = generateConjugations(word)

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Positive, "kilkkaa")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Negative, "kilkkaa")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Positive, "kilkkaisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Negative, "kilkkaisi")

    conjugations.matches(AInfinitive, "kilkkaa")
  }


}
