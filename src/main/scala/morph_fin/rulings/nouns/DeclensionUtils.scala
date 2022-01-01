package morph_fin.rulings.nouns

import morph_fin.*
import morph_fin.kotus_format.Entry
import morph_fin.rulings.*
import morph_fin.rulings.GradationType.{Strong, Weak}
import morph_fin.rulings.morpheme.{Illative, Morphemes, Nominative, Noun, Plural, Singular}
import morph_fin.rulings.nouns.DeclensionUtils.resolve_i_j
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

case class Word(
                 lemma: String,
                 ruleNumber: Int,
                 gradationOpt: Option[Gradation] = None,
                 nominativeReplacement: Option[StructuredWord] = None
               )


object Word {

  def from(lemma: String, ruleNumber: Int, gradationLetterOpt: Option[Char], compoundCase: Boolean = false): Word =
    val gradationOpt = if ruleNumber == 76 then None else gradationLetterOpt.map(GradationHandler.getGradationByLetter(_))
    val (updatedLemma, nominativeReplacement) = if compoundCase then handleCompoundExceptions(lemma) else handleExceptions(lemma)
    Word(updatedLemma, ruleNumber, gradationOpt, nominativeReplacement)

  def handleCompoundExceptions(lemma: String): (String, Option[StructuredWord]) =
    lemma match {
      case "sata" => "sata" -> Some(StructuredWord("sataa", "", ""))
      case "kymmen" => "kymmen" -> Some(StructuredWord("kymmentä", "", ""))
      case _ => lemma -> None
    }


  def handleExceptions(lemma: String): (String, Option[StructuredWord]) =
    lemma match {
      case "seitsemän" => "seitsemä" -> Some(StructuredWord(lemma, "", ""))
      case "kahdeksan" => "kahdeksa" -> Some(StructuredWord(lemma, "", ""))
      case "yhdeksän" => "yhdeksä" -> Some(StructuredWord(lemma, "", ""))
      case "kymmenen" => "kymmen" -> Some(StructuredWord(lemma, "", ""))
      case _ => lemma -> None
    }
}


case class InflectedWord(word: StructuredWord, morphemes: Morphemes, lemma: String, gradationOpt: Option[Gradation] = None){

  def updateGradation(gradationType: GradationType): InflectedWord =
    val gradationString = gradationType match {
      case GradationType.Strong => gradationOpt.map(_.strong).getOrElse("")
      case GradationType.Weak => gradationOpt.map(_.weakValue).getOrElse("")
    }
    val updatedWord = word.copy(gradation = gradationString)
    copy(word = updatedWord)

  @`inline` final def +:*(prefix: String): InflectedWord =
    val updated = word.copy(root = prefix + word.root)
    this.copy(word = updated)

  @`inline` final def *:+(suffix: String): InflectedWord =
    val updated = word.copy(ending = word.ending + suffix)
    this.copy(word = updated)
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
  def generateDeclensions(word: Word)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] = {
    val gradationOpt = word.ruleNumber match {
      case 27 | 31 => word.gradationOpt.orElse(Some(Gradation("t", "d")))
      case 28 => word.gradationOpt.orElse(gradationForStrongLemma(word.lemma))
      case _ => word.gradationOpt
    }
    val rule = findRule(word.lemma, word.ruleNumber)

    //Resolve root
    val (root, lemma) = checkPlurality(rule, word)

    val tsGradation = Seq(27, 28, 31).contains(word.ruleNumber)

    //Resolve gradation
    val rootDividedByGradation = gradationOpt match {
      case Some(gradation) if !rule.isGradation => GradationHandler.splitByGradationLocation(root, gradation, rule.ruleNumber, rule.drop > 0, tsGradation)
      case _ => (root, "")
    }

    rule.cases
      .map(ending  => resolveWord(ending, rootDividedByGradation, lemma, word.copy(gradationOpt = gradationOpt), rule, tsGradation))
      .map(resultWord => updateRule5(resultWord, word.ruleNumber))
  }


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
    val (exceptionalBeginning, gradation) = resolveGradationString(
        word.gradationOpt, ending.gradationTypeOpt, lemma, root._1, root._2, updatedEnding, ending.morphemes
    )

    val resultWord = StructuredWord(exceptionalBeginning.getOrElse(root._1), gradation, root._2 + updatedEnding)
    val updatedWord: StructuredWord = if ending.morphemes.is(Nominative, Singular) && word.nominativeReplacement.nonEmpty
      then word.nominativeReplacement.get else resultWord
    val tsUpdatedWord = if tsGradation then updateTSGradation(updatedWord) else updatedWord

    InflectedWord(tsUpdatedWord, ending.morphemes, word.lemma, word.gradationOpt)
  end resolveWord

  private inline def resolveGradationString(
                                             gradationOpt: Option[Gradation],
                                             gradationTypeOpt: Option[GradationType],
                                             lemma: String,
                                             beforeGradation: String,
                                             afterGradation: String,
                                             updatedEnding: String,
                                             morphemes: Morphemes
                                           ): (Option[String], String) = {
    (gradationOpt, gradationTypeOpt) match {
      case (Some(gradation), Some(Strong)) => None -> gradation.strong
      case (Some(gradation), Some(Weak)) => None -> gradation.weakValue
      case (Some(gradation), _) =>
        val wordGradationType = GradationHandler.getWordGradationTypeForNoun(lemma, gradation)
        val defaultGradationType = GradationHandler.getGradationTypeByEnding(afterGradation + updatedEnding)
        val exceptionGradationType = GradationHandler.resolveNounException(updatedEnding, wordGradationType, morphemes)
        val tpe = exceptionGradationType.getOrElse(defaultGradationType)
        val exceptionalBeginning = resolve_i_j(beforeGradation, lemma, gradation, tpe)
        exceptionalBeginning -> (if (tpe == GradationType.Strong) gradation.strong else gradation.weakValue)
      case (None, _) => None -> ""
    }
  }

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
