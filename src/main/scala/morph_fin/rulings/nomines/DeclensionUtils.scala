package morph_fin.rulings.nomines

import morph_fin.*
import morph_fin.kotus_format.Entry
import morph_fin.rulings.nomines.{Declension, DeclensionRules, Gradation}
import morph_fin.rulings.*
import morph_fin.utils.{Letters, Vocalization}
import MorphemesUtils.*

import java.nio.charset.StandardCharsets


case class StructuredWord(root: String, gradation: String, ending: String) {
  override def toString: String = root + gradation + ending
}

case class Word(lemma: String, ruleNumber: Int, gradation: Option[Gradation])
case class ResultWord(word: StructuredWord, morphemes: Morphemes, lemma: String)

case class PSuffix(info: PossessiveSuffix, ending: String)

object DeclensionUtils {

  import PossessiveSuffix._

  val listOfSomeVowels = Seq('a', 'o', 'u', 'y', 'ä', 'ö')
  val listOfAllVowels = Seq('a', 'o', 'i', 'e', 'u', 'y', 'ä', 'ö')

  def addDeclesionsWithPossessiveSuffixes(rules: Seq[DeclensionRules], word: Word): Seq[ResultWord] =
    val withoutSuffixes = addDeclesions(rules, word)
    withoutSuffixes.flatMap(a => PossessiveSuffixGeneration.addSuffixes(a, word.gradation))

  /**
   * Note: word.lemma can be in plural or singular form. Both cases are handled.
   */
  def addDeclesions(rules: Seq[DeclensionRules], word: Word): Seq[ResultWord] =
    val rule = rules.find(_.ruleNumber == word.ruleNumber).getOrElse(throw new Exception(s"No nomine rule found for: ${word.ruleNumber}"))

    //Resolve root
    val (root, lemma) = checkPlurality(rule, word)
    val updatedRoot = checkRule49(root, rule)

    //Resolve gradation
    val rootDividedByGradation = word.gradation match {
      case Some(gradation) if !rule.isGradation => GradationHandler.splitByGradationLocation(updatedRoot, gradation, rule.ruleNumber)
      case _                                    => (updatedRoot, "")
    }

    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, lemma, word, rule))
      .map(resultWord => updateRule5(resultWord, word.ruleNumber))
  end addDeclesions


  //=========================================================

  private inline def checkPlurality(rule: DeclensionRules, word: Word): (String, String) =
    val singularEnding = rule.findCase(Nom::S).ending
    val pluralEnding = rule.findCase(Nom::P).ending

    if word.lemma.hasEnding(pluralEnding) then
      val vowelMap = Replacement.resolveMap(word.lemma, pluralEnding)

      val lemmaFromPlural = word.lemma.dropRight(pluralEnding.length) + singularEnding.create(vowelMap) //Might have wrong gradation, but it should not matter
      val rootFromPlural = lemmaFromPlural.dropRight(rule.drop)
      rootFromPlural -> lemmaFromPlural
    else
      checkRule5(word, rule) -> word.lemma


  //Example: askele -> askel
  private inline def checkRule49(root: String, rule: DeclensionRules): String =
    if(root.endsWith("e") && rule.ruleNumber == 49) then root.dropRight(1) else root

  //Example: pick-up (has same inflection as risti, ristejä)
  private inline def checkRule5(word: Word, rule: DeclensionRules): String =
    if(word.ruleNumber == 5 && Letters.isConsonant(word.lemma.last)) word.lemma
    else word.lemma.dropRight(rule.drop)

  //Under rule 5 removes wrong 'i' from Nom:S if needed. Example: pick-upi -> pick-up
  private inline def updateRule5(resultWord: ResultWord, ruleNumber: Int): ResultWord =
    if(ruleNumber == 5 && resultWord.morphemes == Nom::S && resultWord.lemma.last != 'i')
    then
      val updatedWord = resultWord.word.copy(ending = "")
      resultWord.copy(word = updatedWord)
    else resultWord

  private inline def resolveWord(ending: Declension, root: (String, String), lemma: String, word: Word, rule: DeclensionRules): ResultWord =
    val updatedEnding = updateEnding(ending, lemma, rule)
    var exceptionalBeginning: Option[String] = None
    val gradation = word.gradation match {
      case Some(gradation) if ending.tpe == NomineGradationType.Strong => gradation.strong
      case Some(gradation) if ending.tpe == NomineGradationType.Weak => gradation.weak
      case Some(gradation) =>
        val wordGradationType = GradationHandler.getWordGradationTypeForNomine(lemma, gradation)
        val defaultGradationType = GradationHandler.getGradationTypeByEnding(root._2 + updatedEnding)
        val exceptionGradationType = GradationHandler.resolveNomineException(lemma, updatedEnding, wordGradationType, ending.morphemes)
        val tpe = exceptionGradationType.getOrElse(defaultGradationType)
        exceptionalBeginning = resolve_i_j(root._1, lemma, gradation, tpe)
        if(tpe == GradationType.Strong) gradation.strong else gradation.weak
      case None            => ""
    }
    val gradationWithApostropheIfNeeded = //Example: laaka -> laa'an
      if gradation.isEmpty && (root._1.lastOption == (root._2 + updatedEnding).headOption) && word.gradation.nonEmpty
      then "\'"
      else gradation

    val resultWord = StructuredWord(exceptionalBeginning.getOrElse(root._1), gradationWithApostropheIfNeeded, root._2 + updatedEnding)
    ResultWord(resultWord, ending.morphemes, word.lemma)
  end resolveWord

  /**
   * Words: aika, poika has i-j variation in consonant gradation.
   * Example: aika -> ajan
   */
  private inline def resolve_i_j(root: String, lemma: String, gradation: Gradation, tpe: GradationType): Option[String] =
    val suitableLemma = lemma != "taika" && lemma != "juhannustaika"
    val suitableEnding = lemma.endsWith("aika") || lemma.endsWith("poika")
    if gradation == Gradation("k", "") &&
      suitableLemma &&
      suitableEnding &&
      tpe == GradationType.Weak then Some(root.dropRight(1) + "j")
    else None

  import morph_fin.utils.VocalizationUtils._


  private inline def updateEnding(ending: Declension, lemma: String, rule: DeclensionRules): String =
    val NomSEnding = rule.cases.find(_.morphemes == Nom::S).get.ending
    val alteredLemma = rule.ruleNumber match {
      case 5 if Letters.isConsonant(lemma.last) => lemma + 'i'
      case _ => lemma
    }
    val replacementMap = Replacement.resolveMap(alteredLemma, NomSEnding)
    var changed = false

    val result: String = ending.ending.map(_ match {
      case RepChar.Rep(c) => changed = true; replacementMap(c).toString
      case RepChar.Ch(c)  => c.toString
    }).mkString("")

    val illativeCondition = Letters.isVowel(lemma.last)
      && ending.morphemes == Ill::S
      && !changed
      && listOfSomeVowels.contains(result.dropRight(1).last)

    val illativeUpdated = if(illativeCondition && result.dropRight(2).isEmpty)
      then result.dropRight(2) + lemma.last + result.last
    else if illativeCondition && result.dropRight(2).nonEmpty
      then result.dropRight(2) + result.dropRight(2).last + result.last
    else result

    val vocalizationUpdated = updateVocalization(illativeUpdated, resolveVocalization(lemma))
    updateEndingCase28(vocalizationUpdated, lemma, rule)
  end updateEnding


  def updateEndingCase28(ending: String, lemma: String, rule: DeclensionRules): String =
    if(rule.ruleNumber == 28 && ending.startsWith("n")) then lemma.dropRight(2).last + ending.drop(1)
    else ending
}