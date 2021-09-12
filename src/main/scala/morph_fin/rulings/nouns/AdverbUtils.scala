package morph_fin.rulings.nouns

import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Comparative, Genitive, Noun, Singular, stiAdverb}
import morph_fin.rulings.rules.DeclensionRule

//TODO untested
object AdverbUtils {

  def generate(singularGenitiveWord: InflectedWord)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    assert(singularGenitiveWord.morphemes.is(Singular, Genitive))

    val word = singularGenitiveWord.word.toString

    val adverb = InflectedWord(StructuredWord(word.dropRight(1), "", "sti"), Noun ~ stiAdverb, singularGenitiveWord.lemma)
    val comp = InflectedWord(StructuredWord(word.dropRight(1), "", "mmin"), Noun ~ stiAdverb ~ Comparative, singularGenitiveWord.lemma)
    Seq(adverb, comp)
}
