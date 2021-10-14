package morph_fin.rulings.nouns

import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Comparative, Genitive, Nominative, Noun, Singular, stiAdverb}
import morph_fin.rulings.rules.DeclensionRule
import morph_fin.utils.Letters

//TODO untested
//TODO add other adverb-forms
//TODO Effect of gradation???
object AdverbUtils {

  def generate(cases: Seq[InflectedWord])(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val genitive = cases.find(_.morphemes.is(Singular, Genitive)).get
    val nominative = cases.find(_.morphemes.is(Singular, Nominative)).get

    val sti = Some(generateSti(genitive))
    val lti = if ltiCondition(genitive.lemma) then Some(generateLti(genitive)) else None
    //val comp = InflectedWord(StructuredWord(word.dropRight(1), "", "mmin"), Noun ~ stiAdverb ~ Comparative, singularGenitiveWord.lemma)
    Seq(sti, lti).flatten

  private inline def generateSti(genitive: InflectedWord) =
    InflectedWord(StructuredWord(genitive.word.toString.dropRight(1), "", "sti"), Noun ~ stiAdverb, genitive.lemma)

  private inline def generateLti(genitive: InflectedWord) =
    InflectedWord(StructuredWord(genitive.word.toString, "", "lti"), Noun ~ stiAdverb, genitive.lemma)

  private inline def ltiCondition(lemma: String) = Letters.isVowel(lemma.last)
}
