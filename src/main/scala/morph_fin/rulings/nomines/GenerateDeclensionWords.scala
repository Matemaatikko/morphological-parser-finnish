package morph_fin.rulings.nomines

import morph_fin.*
import morph_fin.kotus_format.Entry
import morph_fin.rulings.nomines.{Declension, DeclensionRules, Gradation}
import morph_fin.rulings.*

import java.nio.charset.StandardCharsets


case class StructuredWord(root: String, gradation: String, ending: String) {
  override def toString: String = root + gradation + ending
}

case class Word(lemma: String, ruleNumber: Int, gradation: Option[Gradation])
case class ResultWord(word: StructuredWord, morphemes: Morphemes, lemma: String)

case class PSuffix(info: PossessiveSuffix, ending: String)

enum Vocalization:
  case FrontVowel // a, o, u
  case BackVowel //ä, ö, y, i, e


object GenerateDeclensionWords {

  import PossessiveSuffix._
  val suffixes = Seq(
    PSuffix(SingularFirst, "ni"),
    PSuffix(SingularSecond, "si"),
    PSuffix(PluralFirst, "mme"),
    PSuffix(PluralSecond, "nne"),
    PSuffix(Third, "nsa")
  )

  val listOfSomeVowels = Seq('a', 'o', 'u', 'y', 'ä', 'ö')
  val listOfAllVowels = Seq('a', 'o', 'i', 'e', 'u', 'y', 'ä', 'ö')

  def getDeclension(rules: Seq[DeclensionRules], word: Word): DeclensionRules =
    rules.find(_.number == word.ruleNumber).getOrElse(throw new Exception(s"No nomine rule found for: ${word.ruleNumber}"))

  def getRoot(rules: Seq[DeclensionRules], word: Word): String =
    val rule = rules.find(_.number == word.ruleNumber).getOrElse(throw new Exception(s"No nomine rule found for: ${word.ruleNumber}"))
    val pluralEnding = rule.findCase(NomineMorphemes(Case.Nominative, GNumber.Plural))
    val isPluralLemma = word.lemma.endsWith(pluralEnding.ending)
    if isPluralLemma then word.lemma.dropRight(pluralEnding.ending.length) else word.lemma.dropRight(rule.drop)


  def generateWithPossessiveSuffixes(rules: Seq[DeclensionRules], word: Word): Seq[ResultWord] =
    val withoutSuffixes = apply(rules, word)
    withoutSuffixes.flatMap(a => PossessiveSuffixGeneration.addSuffixes(a, word.gradation))
  

  /**
   * Note: word.lemma can be in plural or singular form. Both cases are handled.
   */
  def apply(rules: Seq[DeclensionRules], word: Word): Seq[ResultWord] =
    val rule = rules.find(_.number == word.ruleNumber).getOrElse(throw new Exception(s"No nomine rule found for: ${word.ruleNumber}"))
    val pluralEnding = rule.findCase(NomineMorphemes(Case.Nominative, GNumber.Plural))
    val singularEnding = rule.findCase(NomineMorphemes(Case.Nominative, GNumber.Singular))

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
  end apply

  def resolveWord(ending: Declension, root: (String, String), lemma: String, word: Word): ResultWord =
    val vocalization = resolveVocalization(lemma)
    val lastVowel = if(listOfSomeVowels.contains(lemma.last) && word.ruleNumber != 19) Some(lemma.last) else None
    val updatedEnding = updateEnding(ending, vocalization, lastVowel, root._2, word.gradation.nonEmpty)
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
    val resultWord = StructuredWord(exceptionalBeginning.getOrElse(root._1), gradation, root._2 + updatedEnding)
    ResultWord(resultWord, ending.morphemes, word.lemma)
  end resolveWord

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

  def updateVocalization(ending: String, vocalization: Vocalization): String =
    if vocalization == Vocalization.BackVowel
    then ending.map( _ match {
      case a if a == 'a' => 'ä'
      case a if a == 'o' => 'ö'
      case a if a == 'u' => 'y'
      case a             => a
    })
    else ending.map( _ match {
      case a if a == 'ä' => 'a'
      case a if a == 'ö' => 'o'
      case a if a == 'y' => 'u'
      case a             => a
    })
  /**
   * Example words:
   * tie -> tiessä  (a -> ä in ending)
   * valo -> valoon (a -> o in inessive case)
   * ies -> ikeen (-än as ending (ikeän))
   */
  def updateEnding(ending: Declension, vocalization: Vocalization, lastVowel: Option[Char], rootEnding: String, isGradation: Boolean): String =
    val updatedEnding = updateVocalization(ending.ending, vocalization)

    val updateCondition =
      updatedEnding.length > 1 &&
        listOfSomeVowels.contains(updatedEnding.head) &&
        lastVowel.nonEmpty && ( ending.morphemes == NomineMorphemes(Case.Nominative, GNumber.Singular)) &&
        !isGradation

    val updateCondition2 =
      updatedEnding.length > 1 &&
      listOfSomeVowels.contains(updatedEnding.head) &&
      rootEnding.length == 1 &&
      isGradation

    val vowelUpdated = if updateCondition then
      lastVowel.get.toString + updatedEnding.drop(1)
    else if updateCondition2 then rootEnding + updatedEnding.drop(1)
    else updatedEnding

    val illativeCondition =
      ending.morphemes == NomineMorphemes(Case.Illative, GNumber.Singular) && lastVowel.nonEmpty
    if illativeCondition
    then vowelUpdated.map(c => if listOfSomeVowels.contains(c) then lastVowel.get else c)
    else vowelUpdated

  end updateEnding






}