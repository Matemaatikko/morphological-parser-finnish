package morph_fin.rulings.nouns

import morph_fin.*
import morph_fin.kotus_format.Entry
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Illative, Morphemes, Nominative, Noun, Plural, Singular}
import morph_fin.rulings.rules.*
import morph_fin.utils.{Hyphenation, Letters, Vocalization}

import java.nio.charset.StandardCharsets

/**
* When consonant gradation occurs with weak gradation: "",
* then empty gradation string is replaced with '^' for tracking puposes.
* This marker is removed when word is printed using toString-method.
*/
case class StructuredWord(root: String, gradation: String, ending: String) {
  override def toString: String = Hyphenation.addApostrophes(root + gradation + ending).replace("^", "")
  def value: String = (root + gradation + ending).replace("^", "")
  def append(str: String) = StructuredWord(root, gradation, ending + str)
  def last: Char = (root + gradation + ending).last
}

case class Word(lemma: String, ruleNumber: Int, gradationOpt: Option[Gradation])
case class InflectedWord(word: StructuredWord, morphemes: Morphemes, lemma: String, gradationOpt: Option[Gradation] = None){
  def updateGradation(gradationType: GradationType): InflectedWord =
    val gradationString = gradationType match {
      case GradationType.Strong => gradationOpt.map(_.strong).getOrElse("")
      case GradationType.Weak => gradationOpt.map(_.weakValue).getOrElse("")
    }
    val updatedWord = word.copy(gradation = gradationString)
    copy(word = updatedWord)
}

object DeclensionUtils {


  val listOfSomeVowels = Seq('a', 'o', 'u', 'y', 'ä', 'ö')
  val listOfAllVowels = Seq('a', 'o', 'i', 'e', 'u', 'y', 'ä', 'ö')

  def findRule(lemma: String, number: Int)(using rules: Seq[DeclensionRule]): DeclensionRule =
    val resultOpt = if(number == 49 && lemma.endsWith("e")) rules.find(_.ruleNumber == 492)
    else if (number == 49) rules.find(_.ruleNumber == 491)
    else rules.find(_.ruleNumber == number)
    resultOpt.getOrElse(throw new Exception(s"No noun rule found for: ${number}"))

  def gradationForStrongLemma(lemma: String): Option[Gradation] =
    val value = lemma.dropRight(1).takeRight(2).replace("s", "t")
    GradationHandler.gradationMap.find(_._1 == value).map(a => Gradation(a._1, a._2))

  /**
   * Note: word.lemma can be in plural or singular form. Both cases are handled.
   */
  def generateDeclensions(word: Word)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val gradationOpt = word.ruleNumber match {
      case 27 | 31 => word.gradationOpt.orElse(Some(Gradation("t", "d")))
      case 28 => word.gradationOpt.orElse(gradationForStrongLemma(word.lemma))
      case _  => word.gradationOpt
    }
    val rule = findRule(word.lemma, word.ruleNumber)

    //Resolve root
    val (root, lemma) = checkPlurality(rule, word)

    val tsGradation = Seq(27, 28, 31).contains(word.ruleNumber)

    //Resolve gradation
    val rootDividedByGradation = gradationOpt match {
      case Some(gradation) if !rule.isGradation => GradationHandler.splitByGradationLocation(root, gradation, rule.ruleNumber, rule.drop, tsGradation)
      case _                                    => (root, "")
    }

    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, lemma, word.copy(gradationOpt = gradationOpt), rule, tsGradation))
      .map(resultWord => updateRule5(resultWord, word.ruleNumber))
  end generateDeclensions


  //=========================================================

  private def findCase(rule: DeclensionRule, morphemes: Morphemes): Declension =
    rule.cases.find(ending => ending.morphemes.is(morphemes)).getOrElse(throw new Error("Non-comprehensive matching in rules"))

  private inline def checkPlurality(rule: DeclensionRule, word: Word): (String, String) =
    val singularEnding = findCase(rule, Noun ~ Nominative ~ Singular).ending
    val pluralEnding = findCase(rule, Noun ~ Nominative ~ Plural).ending

    if word.lemma.hasEnding(pluralEnding) then
      val vowelMap = Replacement.resolveMap(word.lemma, pluralEnding)

      val lemmaFromPlural = word.lemma.dropRight(pluralEnding.length) + singularEnding.create(vowelMap) //Might have wrong gradation, but it should not matter
      val rootFromPlural = lemmaFromPlural.dropRight(rule.drop)
      rootFromPlural -> lemmaFromPlural
    else
      checkRule5(word, rule) -> word.lemma


  //Example: askele -> askel
  private inline def checkRule49(root: String, rule: DeclensionRule): String =
    if(root.endsWith("e") && rule.ruleNumber == 49) then root.dropRight(1) else root

  //Example: pick-up (has same inflection as risti, ristejä)
  private inline def checkRule5(word: Word, rule: DeclensionRule): String =
    if(word.ruleNumber == 5 && Letters.isConsonant(word.lemma.last)) word.lemma
    else word.lemma.dropRight(rule.drop)

  //Under rule 5 removes wrong 'i' from Nominative Singular if needed. Example: pick-upi -> pick-up
  private inline def updateRule5(resultWord: InflectedWord, ruleNumber: Int): InflectedWord =
    if(ruleNumber == 5 && resultWord.morphemes.is(Nominative, Singular)&& resultWord.lemma.last != 'i')
    then
      val updatedWord = resultWord.word.copy(ending = "")
      resultWord.copy(word = updatedWord)
    else resultWord

  private inline def resolveWord(
                                  ending: Declension,
                                  root: (String, String),
                                  lemma: String,
                                  word: Word,
                                  rule: DeclensionRule,
                                  tsGradation: Boolean
                                ): InflectedWord =
    import GradationType._
    val updatedEnding = updateEnding(ending, lemma, rule)
    var exceptionalBeginning: Option[String] = None
    val gradation: String = (word.gradationOpt, ending.gradationTypeOpt) match {
      case (Some(gradation), Some(Strong)) => gradation.strong
      case (Some(gradation), Some(Weak)) => gradation.weakValue
      case (Some(gradation), _) =>
        val wordGradationType = GradationHandler.getWordGradationTypeForNoun(lemma, gradation)
        val defaultGradationType = GradationHandler.getGradationTypeByEnding(root._2 + updatedEnding)
        val exceptionGradationType = GradationHandler.resolveNounException(updatedEnding, wordGradationType, ending.morphemes)
        val tpe = exceptionGradationType.getOrElse(defaultGradationType)
        exceptionalBeginning = resolve_i_j(root._1, lemma, gradation, tpe)
        if(tpe == GradationType.Strong) gradation.strong else gradation.weakValue
      case (None, _)            => ""
    }

    val resultWord = StructuredWord(exceptionalBeginning.getOrElse(root._1), gradation, root._2 + updatedEnding)
    val tsUpdatedWord = if tsGradation then updateTSGradation(resultWord) else resultWord
    InflectedWord(tsUpdatedWord, ending.morphemes, word.lemma, word.gradationOpt)
  end resolveWord

  def updateTSGradation(structuredWord: StructuredWord): StructuredWord =
    if(structuredWord.gradation.endsWith("t")  && structuredWord.ending.startsWith("i")) then
      structuredWord.copy(gradation = structuredWord.gradation.replace("t", "s"))
    else if(structuredWord.root.endsWith("k"))
      structuredWord.copy(root = structuredWord.root.dropRight(1) + "h")
    else
      structuredWord

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


  private inline def updateEnding(ending: Declension, lemma: String, rule: DeclensionRule): String =
    val NomSEnding = rule.cases.find(_.morphemes.is(Nominative, Singular)).get.ending
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
      && ending.morphemes.is(Illative, Singular)
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


  def updateEndingCase28(ending: String, lemma: String, rule: DeclensionRule): String =
    if(rule.ruleNumber == 28 && ending.startsWith("n")) then lemma.dropRight(2).last + ending.drop(1)
    else ending
}