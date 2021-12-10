package morph_fin.rulings.verbs

import morph_fin.rulings.nouns.{DeclensionUtils, InflectedWord, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.morpheme._
import morph_fin.rulings.rules.{ConjugationRule, LoadAndParseVerbRules}

class ConjugationUtilsTest extends AnyFlatSpec with should.Matchers {

  extension[A] (a: A)
    def opt = Some(a)


  import ConjugationUtils._
  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes.is(morphemes)).map(_.word.toString).toSet == words.toSet, morphemes)

  "generateConjugations" should "handle word: saunoa" in {
    val word = Word("saunoa", 52, None)
    val conjugations = generateConjugations(word)

    // Present

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive, "saunon")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularSecond ~ Positive, "saunot")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Positive, "saunoo")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralFirst ~ Positive, "saunomme")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralSecond ~ Positive, "saunotte")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralThird ~ Positive, "saunovat")

    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Positive, "saunotaan")

    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularFirst ~ Negative, "sauno")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularSecond ~ Negative, "sauno")
    conjugations.matches(Finite ~ Indicative ~ Present ~ SingularThird ~ Negative, "sauno")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralFirst ~ Negative, "sauno")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralSecond ~ Negative, "sauno")
    conjugations.matches(Finite ~ Indicative ~ Present ~ PluralThird  ~ Negative, "sauno")

    conjugations.matches(Finite ~ Indicative ~ Present ~ Passive ~ Negative, "saunota")

    // Imperfect

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularFirst ~ Positive, "saunoin")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularSecond ~ Positive, "saunoit")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ SingularThird ~ Positive, "saunoi")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ PluralFirst ~ Positive, "saunoimme")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ PluralSecond ~ Positive, "saunoitte")
    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ PluralThird  ~ Positive, "saunoivat")

    conjugations.matches(Finite ~ Indicative ~ Imperfect ~ Passive ~ Positive, "saunottiin")

    // General-form
    conjugations.matches(Finite ~ General ~ Active ~ Singular, "saunonut")
    conjugations.matches(Finite ~ General ~ Active ~ Plural, "saunoneet")
    conjugations.matches(Finite ~ General ~ Passive, "saunottu")

    // Potential

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Positive, "saunonen")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularSecond ~ Positive, "saunonet")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird  ~ Positive, "saunonee")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralFirst ~ Positive, "saunonemme")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralSecond ~ Positive, "saunonette")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralThird ~ Positive, "saunonevat")

    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Positive, "saunottaneen")

    conjugations.matches(Finite ~ Potential ~ Present ~ SingularFirst ~ Negative, "saunone")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularSecond ~ Negative, "saunone")
    conjugations.matches(Finite ~ Potential ~ Present ~ SingularThird ~ Negative, "saunone")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralFirst ~ Negative, "saunone")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralSecond ~ Negative, "saunone")
    conjugations.matches(Finite ~ Potential ~ Present ~ PluralThird ~ Negative, "saunone")

    conjugations.matches(Finite ~ Potential ~ Present ~ Passive ~ Negative, "saunottane")

    // Conditional

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Positive, "saunoisin")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularSecond ~ Positive, "saunoisit")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Positive, "saunoisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Positive, "saunoisimme")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralSecond ~ Positive, "saunoisitte")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralThird ~ Positive, "saunoisivat")

    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Positive, "saunottaisiin")

    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularFirst ~ Negative, "saunoisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularSecond ~ Negative, "saunoisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ SingularThird ~ Negative, "saunoisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralFirst ~ Negative, "saunoisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralSecond ~ Negative, "saunoisi")
    conjugations.matches(Finite ~ Conditional ~ Present ~ PluralThird ~ Negative, "saunoisi")

    conjugations.matches(Finite ~ Conditional ~ Present ~ Passive ~ Negative, "saunottaisi")

    // Imperative

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Positive, "sauno")
    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularThird ~ Positive, "saunokoon")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralFirst ~ Positive, "saunokaamme")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Positive, "saunokaa")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Positive, "saunokoot")

    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Positive, "saunottakoon")

    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularSecond ~ Negative, "sauno")
    conjugations.matches(Finite ~ Imperative ~ Present ~ SingularThird ~ Negative, "saunoko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralFirst ~ Negative, "saunoko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralSecond ~ Negative, "saunoko")
    conjugations.matches(Finite ~ Imperative ~ Present ~ PluralThird ~ Negative, "saunoko")

    conjugations.matches(Finite ~ Imperative ~ Present ~ Passive ~ Negative, "saunottako")

    // Nominal forms

    conjugations.matches(AInfinitive, "saunoa")
    conjugations.matches(AInfinitiveLong, "saunoakse")

    conjugations.matches(EInfinitive ~ Inessive ~ Active, "saunoessa")
    conjugations.matches(EInfinitive ~ Inessive ~ Passive, "saunottaessa")
    conjugations.matches(EInfinitive ~ Instructive ~ Active, "saunoen")

    conjugations.matches(MAInfinitive ~ Inessive ~ Active, "saunomassa")
    conjugations.matches(MAInfinitive ~ Elative ~ Active, "saunomasta")
    conjugations.matches(MAInfinitive ~ Illative ~ Active, "saunomaan")
    conjugations.matches(MAInfinitive ~ Adessive ~ Active, "saunomalla")
    conjugations.matches(MAInfinitive ~ Abessive ~ Active, "saunomatta")
    conjugations.matches(MAInfinitive ~ Instructive ~ Active, "saunoman")
    conjugations.matches(MAInfinitive ~ Instructive ~ Passive, "saunottaman")

    conjugations.matches(InfinitiveIV, "saunominen")
    conjugations.matches(InfinitiveV, "saunomaisilla")
    conjugations.matches(InfinitiveVI, "saunovina")
    conjugations.matches(InfinitiveVII, "saunottavissa")

    conjugations.matches(vAParticiple ~ Active, "saunova")
    conjugations.matches(vAParticiple ~ Passive, "saunottava")
    conjugations.matches(nUtParticiple, "saunonut")
    conjugations.matches(tUParticiple, "saunottu")
    conjugations.matches(AgentParticiple, "saunoma")
    conjugations.matches(NegativeParticiple, "saunomaton")
  }
}
