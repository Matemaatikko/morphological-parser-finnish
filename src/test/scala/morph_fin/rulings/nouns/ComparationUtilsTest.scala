package morph_fin.rulings.nouns

import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.*
import morph_fin.rulings.rules.{DeclensionRule, Gradation, LoadAndParseNomineRules}

extension[A] (a: A)
  def opt = Some(a)

class ComparationUtilsTest extends AnyFlatSpec with should.Matchers {

  import ComparationUtils._
  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules

  def matchesComp(word: Word, expected: String*) =
    val gen = DeclensionUtils.generateDeclensions(word).find(_.morphemes.is(Genitive, Singular)).get
    val resultWords = generateComparativeWord(gen)
    assert(resultWords.toSet == expected.toSet)

  def matchesSup(word: Word, expected: String*) =
    val gen = DeclensionUtils.generateDeclensions(word).find(_.morphemes.is(Genitive, Singular)).get
    val resultWords = generateSuperlativeWord(gen)
    assert(resultWords.toSet == expected.toSet)

  "ComparationUtils" should "work for -kaunis-" in {
    val word = Word("kaunis", 41, None)
    matchesComp(word, "kauniimpi")
    matchesSup(word, "kaunein")
  }

  it should "work for -keltainen-" in {
    val word = Word("keltainen", 38, None)
    matchesComp(word, "keltaisempi")
    matchesSup(word, "keltaisin")
  }

  it should "work for -kiva-" in {
    val word = Word("kiva", 9, None)
    matchesComp(word, "kivampi", "kivempi")
    matchesSup(word, "kivoin", "kivin")
  }

  it should "work for -hyvä-" in {
    val word = Word("hyvä", 10, None)
    matchesComp(word, "parempi")
    matchesSup(word, "paras")
  }

  it should "work for -solakka-" in {
    val word = Word("solakka", 14, Gradation("kk", "k").opt)
    matchesComp(word, "solakampi")
    matchesSup(word, "solakoin", "solakin")
  }

  it should "work for -halpa-" in {
    val word = Word("halpa", 9, Gradation("p", "v").opt)
    matchesComp(word, "halvempi")
    matchesSup(word, "halvin", "halvoin")
  }

  it should "work for -aito-" in {
    val word = Word("aito", 1, Gradation("t", "d").opt)
    matchesComp(word, "aidompi")
    matchesSup(word, "aidoin")
  }


}
