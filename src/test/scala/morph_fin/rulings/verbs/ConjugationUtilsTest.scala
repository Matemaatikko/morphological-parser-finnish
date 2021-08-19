package morph_fin.rulings.verbs

import morph_fin.rulings.Morphemes
import morph_fin.rulings.nomines.{DeclensionUtils, Gradation, ResultWord, Word}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.Morphemes

extension[A] (a: A)
  def opt = Some(a)


class ConjugationUtilsTest extends AnyFlatSpec with should.Matchers {

  import morph_fin.rulings.MorphemesUtils._

  import ConjugationUtils._
  val rules = LoadAndParseVerbRules.rules

  extension(list: Seq[ResultWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes == morphemes).map(_.word.toString).toSet == words.toSet, morphemes)

  "addConjugations" should "handle case: rule: 1, word: järkky" in {
    val word = Word("järkky", 1, Gradation("kk", "k").opt)
    val declensions = generateConjugations(rules, word)

  }
}
