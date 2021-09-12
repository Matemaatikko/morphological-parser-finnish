package morph_fin.rulings.verbs

import morph_fin.rulings.nouns.{DeclensionUtils, InflectedWord, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.morpheme.Morphemes
import morph_fin.rulings.rules.LoadAndParseVerbRules

extension[A] (a: A)
  def opt = Some(a)


class ConjugationUtilsTest extends AnyFlatSpec with should.Matchers {


  import ConjugationUtils._
  val rules = LoadAndParseVerbRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes == morphemes).map(_.word.toString).toSet == words.toSet, morphemes)

//  "addConjugations" should "handle case: rule: 1, word: järkky" in {
//    val word = Word("järkky", 1, Gradation("kk", "k").opt)
//    val declensions = generateConjugations(rules, word)
//
//  }
}
