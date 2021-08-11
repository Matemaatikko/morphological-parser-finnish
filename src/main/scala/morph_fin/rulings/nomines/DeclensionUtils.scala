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
    val updatedRoot = checkRule49(root)

    //Resolve gradation
    val rootDividedByGradation = word.gradation match {
      case Some(gradation) if !rule.isGradation => GradationHandler.splitByGradationLocation(updatedRoot, gradation)
      case _                                    => (updatedRoot, "")
    }

    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, lemma, word, rule))
      .map(resultWord => updateRule5(resultWord, word.ruleNumber))
  end addDeclesions


  //=========================================================

  private inline def checkPlurality(rule: DeclensionRules, word: Word): (String, String) =
    val pluralEnding = rule.findCase(Nom::P)
    val singularEnding = rule.findCase(Nom::S)

    val isPluralLemma = word.lemma.endsWith(pluralEnding.ending)
    val lemmaFromPlural = word.lemma.dropRight(pluralEnding.ending.length) + singularEnding.ending //Might have wrong gradation.
    val rootFromPlural = lemmaFromPlural.dropRight(rule.drop)
    val root = if isPluralLemma then rootFromPlural else checkRule5(word, rule)
    val lemma = if isPluralLemma then lemmaFromPlural else word.lemma
    (root, lemma)

  //Example: askele -> askel
  private inline def checkRule49(root: String): String =
    if(root.endsWith("e")) then root.dropRight(1) else root

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
    val replacementVowels = lemma.takeRight(rule.drop).takeWhile(Letters.isVowel(_))
    assert(rule.replacementVowels.length == replacementVowels.length)
    val replacementMap = (0 until replacementVowels.length).map(i => rule.replacementVowels(i) -> replacementVowels(i)).toMap

    val result = ending.ending.map(_ match {
      case RepChar.Rep(c) => replacementMap(c).toString
      case RepChar.Ch(c)  => c.toString
    }).mkString("")

    updateVocalization(result, resolveVocalization(lemma))
  end updateEnding

}