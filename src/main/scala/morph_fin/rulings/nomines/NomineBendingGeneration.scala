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

    //Resolve root
    val isPluralLemma = word.lemma.endsWith(pluralEnding.ending)
    val root = if isPluralLemma then word.lemma.dropRight(pluralEnding.ending.length) else word.lemma.dropRight(rule.drop)

    //Resolve gradation
    val rootDividedByGradation = word.gradation match {
      case Some(gradation) if !rule.isGradation => GradationHandler.splitByGradationLocation(root, gradation)
      case _                                    => (root, "")
    }

    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, word))


  def resolveWord(ending: NomineEnding, root: (String, String), word: Word): ResultWord =
    val vocalization = resolveVocalization(word.lemma)
    val lastVowel = if(listOfSomeVowels.contains(word.lemma.last) && word.ruleNumber != 19) Some(word.lemma.last) else None
    val updatedEnding = updateEnding(ending, vocalization, lastVowel, word.gradation.nonEmpty)
    val gradation = word.gradation match {
      case Some(gradation) =>
        val tpe = GradationHandler.resolveGradationType(word.lemma.last, ending.morphemes)
        if(tpe == GradationType.Strong) gradation.strong else gradation.weak
      case None            => ""
    }
    val resultWord = root._1 + gradation + root._2 + updatedEnding
    ResultWord(resultWord, ending.morphemes, word.lemma)

  def resolveVocalization(lemma: String): Vocalization =
    if lemma.forall(char => char != 'a' && char != 'o' && char != 'u')
    then Vocalization.BackVowel
    else Vocalization.FrontVowel


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