package morph_fin.rulings.nomines

import morph_fin.*
import morph_fin.kotus_format.Entry
import morph_fin.rulings.nomines.{NomineEnding, Gradation, NomineBending}
import morph_fin.rulings.*

import java.nio.charset.StandardCharsets


case class Word(lemma: String, ruleNumber: Int, gradation: Option[Gradation])
case class ResultWord(word: String, morphemes: NomineMorphemes, lemma: String)

enum Vocalization:
  case FrontVowel // a, o, u
  case BackVowel //ä, ö, y, i, e


/**
 * Method generates bendings for given word: Word.
 * Example: säde
 *
 */
object GenerateNomineBendings {

  val listOfSomeVowels = Seq('a', 'o', 'u', 'y', 'ä', 'ö')
  val listOfAllVowels = Seq('a', 'o', 'i', 'e', 'u', 'y', 'ä', 'ö')

  def getBending(rules: Seq[NomineBending], word: Word): NomineBending =
    rules.find(_.number == word.ruleNumber).getOrElse(throw new Exception(s"No nomine rule found for: ${word.ruleNumber}"))

  def getRoot(rules: Seq[NomineBending], word: Word): String =
    val rule = rules.find(_.number == word.ruleNumber).getOrElse(throw new Exception(s"No nomine rule found for: ${word.ruleNumber}"))
    val pluralEnding = rule.findCase(NomineMorphemes(Case.Nominative, Form.Plural))
    val isPluralLemma = word.lemma.endsWith(pluralEnding.ending)
    if isPluralLemma then word.lemma.dropRight(pluralEnding.ending.length) else word.lemma.dropRight(rule.drop)

  /**
   * Note: word.lemma can be in plural or singular form. Both cases are handled.
   */
  def apply(rules: Seq[NomineBending], word: Word): Seq[ResultWord] =
    val rule = rules.find(_.number == word.ruleNumber).getOrElse(throw new Exception(s"No nomine rule found for: ${word.ruleNumber}"))
    val pluralEnding = rule.findCase(NomineMorphemes(Case.Nominative, Form.Plural))
    val singularEnding = rule.findCase(NomineMorphemes(Case.Nominative, Form.Singular))

    //Resolve root
    val isPluralLemma = word.lemma.endsWith(pluralEnding.ending)
    val lemmaFromPlural = word.lemma.dropRight(pluralEnding.ending.length) + singularEnding.ending //Might have wrong gradation.
    val rootFromPlural = lemmaFromPlural.dropRight(rule.drop)
    val root = if isPluralLemma then rootFromPlural else word.lemma.dropRight(rule.drop)
    val lemma = if isPluralLemma then lemmaFromPlural else word.lemma

    //Resolve gradation
    val rootDividedByGradation = word.gradation match {
      case Some(gradation) if !rule.isGradation => GradationHandler.splitByGradationLocation(root, gradation)
      case _                                    => (root, "")
    }

    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, lemma, word))


  def resolveWord(ending: NomineEnding, root: (String, String), lemma: String, word: Word): ResultWord =
    val vocalization = resolveVocalization(lemma)
    val lastVowel = if(listOfSomeVowels.contains(lemma.last) && word.ruleNumber != 19) Some(lemma.last) else None
    val updatedEnding = updateEnding(ending, vocalization, lastVowel, word.gradation.nonEmpty)
    var exceptionalBeginning: Option[String] = None
    val gradation = word.gradation match {
      case Some(gradation) if ending.tpe == NomineGradationType.Strong => gradation.strong
      case Some(gradation) if ending.tpe == NomineGradationType.Weak => gradation.weak
      case Some(gradation) =>
        val wordGradationType = GradationHandler.getWordGradationTypeForNomine(lemma, gradation)
        val defaultGradationType = GradationHandler.gradationTypeByEnding(root._2 + updatedEnding)
        val exceptionGradationType = GradationHandler.resolveNomineException(lemma, updatedEnding, wordGradationType, ending.morphemes)
        val tpe = exceptionGradationType.getOrElse(defaultGradationType)
        exceptionalBeginning = resolve_i_j(root._1, lemma, gradation, tpe)
        if(tpe == GradationType.Strong) gradation.strong else gradation.weak
      case None            => ""
    }
    val resultWord = exceptionalBeginning.getOrElse(root._1) + gradation + root._2 + updatedEnding
    ResultWord(resultWord, ending.morphemes, word.lemma)

  def resolveVocalization(lemma: String): Vocalization =
    if lemma.forall(char => char != 'a' && char != 'o' && char != 'u')
    then Vocalization.BackVowel
    else Vocalization.FrontVowel

  /**
   * Words: aika, poika has i-j variation in consonant gradation.
   * Example: aika -> ajan
   */
  def resolve_i_j(root: String, lemma: String, gradation: Gradation, tpe: GradationType): Option[String] =
    val suitableLemma = lemma != "taika" && lemma != "juhannustaika"
    val suitableEnding = lemma.endsWith("aika") || lemma.endsWith("poika")
    if gradation == Gradation("k", "") &&
      suitableLemma &&
      suitableEnding &&
      tpe == GradationType.Weak then Some(root.dropRight(1) + "j")
    else None


  /**
   * Example words:
   * tie -> tiessä  (a -> ä in ending)
   * valo -> valoon (a -> o in inessive case)
   * -- TODO need the example for the missing case --
   */
  def updateEnding(ending: NomineEnding, vocalization: Vocalization, lastVowel: Option[Char], isGradation: Boolean): String =
    val updatedEnding =
      if vocalization == Vocalization.BackVowel
      then ending.ending.map( _ match {
        case a if a == 'a' => 'ä'
        case a if a == 'o' => 'ö'
        case a if a == 'u' => 'y'
        case a             => a
      })
      else ending.ending

    val updateCondition =
      updatedEnding.length > 1 &&
        listOfSomeVowels.contains(updatedEnding.head) &&
        lastVowel.nonEmpty && ( ending.morphemes == NomineMorphemes(Case.Nominative, Form.Singular)) &&
        !isGradation

    val vowelUpdated = if updateCondition then
      lastVowel.get.toString + updatedEnding.drop(1)
    else updatedEnding

    val illativeCondition =
      ending.morphemes == NomineMorphemes(Case.Illative, Form.Singular) && lastVowel.nonEmpty
    if illativeCondition
    then vowelUpdated.map(c => if listOfSomeVowels.contains(c) then lastVowel.get else c)
    else vowelUpdated

  end updateEnding






}