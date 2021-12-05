package morph_fin.rulings.nouns

import morph_fin.kotus_format.LoadUpdatedKotus
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNounRules}
import morph_fin.kotus_format.*
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.*
import morph_fin.utils.TablePrinter


case class Filters(
                  superlative: Boolean = true,
                  comparative: Boolean = true,
                  singular: Boolean = true,
                  plural: Boolean = true,
                  posessiveSuffix: Boolean = true,
                  adverb: Boolean = true
                  )


class NounPrinter {

  def valueOf(x: (Morpheme, Morpheme)): Int = MorphemeOrdering.valueMap(x._1) + MorphemeOrdering.valueMap(x._2)

  given Seq[DeclensionRule] = LoadAndParseNounRules.rules

  val words = LoadUpdatedKotus.apply()

  def print(lemma: String, filters: Filters = Filters()): String =
    val resultOpt = words.find(_.value == lemma)

    resultOpt match {
      case Some(UpdatedWord.StandardBending(lemma, inflection)) =>
        val word = Word.from(lemma, inflection.rule, inflection.gradationLetter)
        val declensions = AllDeclensionUtils.generateAllDeclections(word)
        resolve(declensions, filters)
      case Some(UpdatedWord.Compound2(lemma, prefix, suffix)) =>
        val declensions = AllDeclensionUtils.generateDeclectionsForCompound(lemma, prefix, suffix)
        resolve(declensions, filters)
      case _ => ""
    }


  def resolve(declensions: Seq[InflectedWord], filters: Filters): String =
    val filteredDeclensions = declensions
      .filter(a => filters.singular || a.morphemes.isNot(Singular))
      .filter(a => filters.plural || a.morphemes.isNot(Plural))
      .filter(a => filters.posessiveSuffix || a.morphemes.isNotPossessiveSuffix)

    val normal = filteredDeclensions.filter(_.morphemes.isNotAny(Comparative, Superlative, stiAdverb))
      .groupBy(a => (a.morphemes.getCase.get, a.morphemes.getGrammaticalNumber.get))
      .toSeq
      .sortBy(a => valueOf(a._1))

    val comparative = filteredDeclensions.filter(a => a.morphemes.is(Comparative) && a.morphemes.isNot(stiAdverb))
      .groupBy(a => (a.morphemes.getCase.get, a.morphemes.getGrammaticalNumber.get))
      .toSeq
      .sortBy(a => valueOf(a._1))

    val superlative = filteredDeclensions.filter(_.morphemes.is(Superlative))
      .groupBy(a => (a.morphemes.getCase.get, a.morphemes.getGrammaticalNumber.get))
      .toSeq
      .sortBy(a => valueOf(a._1))

    val adverb = filteredDeclensions.filter(_.morphemes.is(stiAdverb))

    given Boolean = filters.posessiveSuffix

    formTableString(normal)
      + (if(filters.comparative) "\n\nComparative: \n" + formTableString(comparative) else "")
      + (if(filters.superlative) "\n\nSuperlative: \n" + formTableString(superlative)  else "")
      + (if(filters.adverb) "\n\nsti-adverb: " + adverb.map(_.word.toString).mkString(", ")  else "")
  end resolve

  def formTableString(list: Seq[((Morpheme, Morpheme), Seq[InflectedWord])])(using possessiveSuffix: Boolean): String =
    val table = list.toSeq.map(a =>
      val ((cse, number), list) = a

      val a1 = PrintMorphemes.apply(Noun ~ cse ~ number)
      val a2 = list.filter(_.morphemes.isNotPossessiveSuffix).map(_.word.toString()).mkString(", ")
      val a3 = list.filter(_.morphemes.is(PossessiveSuffix.SingularFirst)).map(_.word.toString()).mkString(", ")
      val a4 = list.filter(_.morphemes.is(PossessiveSuffix.SingularSecond)).map(_.word.toString()).mkString(", ")
      val a5 = list.filter(_.morphemes.is(PossessiveSuffix.PluralFirst)).map(_.word.toString()).mkString(", ")
      val a6 = list.filter(_.morphemes.is(PossessiveSuffix.PluralSecond)).map(_.word.toString()).mkString(", ")
      val a7 = list.filter(_.morphemes.is(PossessiveSuffix.ThirdPos)).map(_.word.toString()).mkString(", ")
      if(possessiveSuffix) Seq(a1, a2, a3, a4, a5, a6, a7) else Seq(a1, a2)
    )
    TablePrinter.printTable(table)
  
}
