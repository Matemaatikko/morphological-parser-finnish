package morph_fin.rulings.nouns

import morph_fin.kotus_format.LoadUpdatedKotus
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNomineRules}
import morph_fin.kotus_format.*
import morph_fin.rulings.*
import morph_fin.rulings.morpheme._


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

  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules

  val words = LoadUpdatedKotus.apply()
    .filter(_.isInstanceOf[UpdatedWord.StandardBending])
    .map(_.asInstanceOf[UpdatedWord.StandardBending])

  def print(lemma: String, filters: Filters = Filters()): String =
    val resultOpt = words.find(_.word == lemma)

    resultOpt match {
      case Some(UpdatedWord.StandardBending(lemma, inflection)) =>
        val word = Word.from(lemma, inflection.rule, inflection.gradationLetter)
        resolve(word, filters)
      case _ => ""
    }


  def resolve(word: Word, filters: Filters): String =
    val declensions = AllDeclensionUtils.generateAllDeclections(word)
      .filter(a => filters.singular || a.morphemes.isNot(Singular))
      .filter(a => filters.plural || a.morphemes.isNot(Plural))
      .filter(a => filters.posessiveSuffix || a.morphemes.isNotPossessiveSuffix)

    val normal = declensions.filter(_.morphemes.isNotAny(Comparative, Superlative, stiAdverb))
      .groupBy(a => (a.morphemes.getCase.get, a.morphemes.getGrammaticalNumber.get))
      .toSeq
      .sortBy(a => valueOf(a._1))

    val comparative = declensions.filter(a => a.morphemes.is(Comparative) && a.morphemes.isNot(stiAdverb))
      .groupBy(a => (a.morphemes.getCase.get, a.morphemes.getGrammaticalNumber.get))
      .toSeq
      .sortBy(a => valueOf(a._1))

    val superlative = declensions.filter(_.morphemes.is(Superlative))
      .groupBy(a => (a.morphemes.getCase.get, a.morphemes.getGrammaticalNumber.get))
      .toSeq
      .sortBy(a => valueOf(a._1))

    val adverb = declensions.filter(_.morphemes.is(stiAdverb))

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
    printTable(table)


  //It is assumed that listOfRows is table.
  def printTable(listOfRows: Seq[Seq[String]]): String =
    val numberOfRows = listOfRows.length
    val numberOfColumns = listOfRows(0).length

    val maxLenghtsInColumns: Seq[Int] = for(i <- 0 until numberOfColumns) yield {
      var max = 0
      for(j <- 0 until numberOfRows) {
        val elem = listOfRows(j)(i)
        if(elem.length > max) max = elem.length
      }
      max
    }

    val lineSeparator = "-"*(maxLenghtsInColumns.sum + 3*numberOfColumns)

    (for(j <- 0 until numberOfRows) yield {
      val row = listOfRows(j)
      val filled = for(i <- 0 until numberOfColumns) yield {
        val elem = row(i)
        elem + " "*(maxLenghtsInColumns(i) - elem.length)
      }
      filled.mkString("| ", " | ", " |\n")
    }).mkString(lineSeparator+"\n", lineSeparator+"\n", lineSeparator)

}
