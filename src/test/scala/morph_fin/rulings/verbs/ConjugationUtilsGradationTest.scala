package morph_fin.rulings.verbs

import morph_fin.rulings.nouns.{DeclensionUtils, InflectedWord, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.morpheme.*
import morph_fin.rulings.rules.{ConjugationRule, Gradation, LoadAndParseVerbRules}


class ConjugationUtilsGradationTest extends AnyFlatSpec with should.Matchers {

  extension[A] (a: A)
    def opt = Some(a)


  import ConjugationUtils._
  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes.is(morphemes)).map(_.word.toString).toSet == words.toSet, morphemes)

  "generateConjugations" should "handle word: kirkua" in {
    val word = Word("kirkua", 52, Some(Gradation("k", "")))
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "kirun")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularSecond ~ Positive, "kirut")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Positive, "kirkuu")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralFirst ~ Positive, "kirumme")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralSecond ~ Positive, "kirutte")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralThird ~ Positive, "kirkuvat")

    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "kirutaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "kiru")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularSecond ~ Negative, "kiru")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Negative, "kiru")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralFirst ~ Negative, "kiru")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralSecond ~ Negative, "kiru")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralThird  ~ Negative, "kiru")

    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "kiruta")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularFirst ~ Positive, "kiruin")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "kiruit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularThird ~ Positive, "kirkui")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ PluralFirst ~ Positive, "kiruimme")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ PluralSecond ~ Positive, "kiruitte")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ PluralThird  ~ Positive, "kirkuivat")

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "kiruttiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "kirkunut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "kirkuneet")
    conjugations.matches(Finite ~ General ~ Passive, "kiruttu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Positive, "kirkunen")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularSecond ~ Positive, "kirkunet")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "kirkunee")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralFirst ~ Positive, "kirkunemme")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralSecond ~ Positive, "kirkunette")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralThird ~ Positive, "kirkunevat")

    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "kiruttaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "kirkune")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularSecond ~ Negative, "kirkune")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird ~ Negative, "kirkune")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralFirst ~ Negative, "kirkune")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralSecond ~ Negative, "kirkune")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralThird ~ Negative, "kirkune")

    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "kiruttane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Positive, "kirkuisin")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularSecond ~ Positive, "kirkuisit")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Positive, "kirkuisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "kirkuisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralSecond ~ Positive, "kirkuisitte")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralThird ~ Positive, "kirkuisivat")

    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "kiruttaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "kirkuisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularSecond ~ Negative, "kirkuisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Negative, "kirkuisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Negative, "kirkuisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralSecond ~ Negative, "kirkuisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralThird ~ Negative, "kirkuisi")

    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "kiruttaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Positive, "kiru")
    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularThird ~ Positive, "kirkukoon")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralFirst ~ Positive, "kirkukaamme")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "kirkukaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Positive, "kirkukoot")

    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "kiruttakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "kiru")
    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularThird ~ Negative, "kirkuko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralFirst ~ Negative, "kirkuko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Negative, "kirkuko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "kirkuko")

    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "kiruttako")

    // Nominal forms

    conjugations.matches(AInfinitive, "kirkua")
    conjugations.matches(AInfinitiveLong, "kirkuakse")

    conjugations.matches(EInfinitive ~ Inessive ~ Active, "kirkuessa")
    conjugations.matches(EInfinitive ~ Inessive ~ Passive, "kiruttaessa")
    conjugations.matches(EInfinitive ~ Instructive ~ Active, "kirkuen")

    conjugations.matches(MAInfinitive ~ Inessive ~ Active, "kirkumassa")
    conjugations.matches(MAInfinitive ~ Elative ~ Active, "kirkumasta")
    conjugations.matches(MAInfinitive ~ Illative ~ Active, "kirkumaan")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "kirkumalla")
    conjugations.matches(MAInfinitive ~ Abessive ~ Active, "kirkumatta")
    conjugations.matches(MAInfinitive ~ Instructive ~ Active, "kirkuman")
    conjugations.matches(MAInfinitive ~ Instructive ~ Passive, "kiruttaman")

    conjugations.matches(InfinitiveIV, "kirkuminen")
    conjugations.matches(InfinitiveV, "kirkumaisilla")
    conjugations.matches(InfinitiveVI, "kirkuvina")
    conjugations.matches(InfinitiveVII, "kiruttavissa")

    conjugations.matches(vAParticiple ~ Active, "kirkuva")
    conjugations.matches(vAParticiple ~ Passive, "kiruttava")
    conjugations.matches(nUtParticiple, "kirkunut")
    conjugations.matches(tUParticiple, "kiruttu")
    conjugations.matches(AgentParticiple, "kirkuma")
    conjugations.matches(NegativeParticiple, "kirkumaton")
  }
}
