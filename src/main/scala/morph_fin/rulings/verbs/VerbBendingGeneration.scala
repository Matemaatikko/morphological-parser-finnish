package morph_fin.rulings.verbs

import morph_fin.rulings.NomineMorphemes
import morph_fin.rulings.nomines._
import morph_fin.rulings._
import morph_fin.rulings.GradationHandler

object GenerateVerbBendings {


  def apply(rules: Seq[VerbBending], word: Word): Seq[ResultWord] =
    val rule = rules.find(_.ruleNumber == word.ruleNumber).getOrElse(throw new Exception(s"No verb rule found for: ${word.ruleNumber}"))

    //Resolve root
    val root = word.lemma.dropRight(rule.drop)

    //Resolve gradation
    val rootDividedByGradation = word.gradation match {
      case Some(gradation) if !rule.isGradation => GradationHandler.splitByGradationLocation(root, gradation)
      case _                                    => (root, "")
    }
    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, word))
  end apply

  def resolveWord(ending: VerbEnding, root: (String, String), word: Word): ResultWord =
    val vocalization = resolveVocalization(word.lemma)
    val updatedEnding = updateEnding(ending, vocalization)
    val gradation = word.gradation match {
      case Some(gradation) if ending.tpe == NomineGradationType.Strong => gradation.strong
      case Some(gradation) if ending.tpe == NomineGradationType.Weak => gradation.weak
      case Some(gradation) =>
        val wordGradationType = GradationHandler.getWordGradationTypeForVerb(word.lemma, gradation)
        val defaultGradationType = GradationHandler.getGradationTypeByEnding(root._2 + updatedEnding)
        val exceptionGradationType = GradationHandler.resolveVerbException(word.lemma, wordGradationType, ending.morphemes)
        val tpe = exceptionGradationType.getOrElse(defaultGradationType)
        if(tpe == GradationType.Strong) gradation.strong else gradation.weak
      case None            => ""
    }
    val resultWord = handleSpecialCase(root._1 + gradation + root._2 + updatedEnding, ending.morphemes)
    ResultWord(resultWord, ending.morphemes, word.lemma)
  end resolveWord


  val IndPreS3 = VerbMophemes.Standard(Modus.Indicative, Tempus.Presens, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Positive)

  def handleSpecialCase(word: String, morphemes: Morphemes): String =
    if morphemes == IndPreS3 then
      val dropped = word.dropRight(1)
      val char = dropped.last
      dropped + char 
    else word

  def resolveVocalization(lemma: String): Vocalization =
    if lemma.forall(char => char != 'a' && char != 'o' && char != 'u')
    then Vocalization.BackVowel
    else Vocalization.FrontVowel


  def updateEnding(ending: VerbEnding, vocalization: Vocalization): String =
    if vocalization == Vocalization.BackVowel
    then ending.ending.map( _ match {
      case a if a == 'a' => 'ä'
      case a if a == 'o' => 'ö'
      case a if a == 'u' => 'y'
      case a             => a
    })
    else ending.ending.map( _ match {
      case a if a == 'ä' => 'a'
      case a if a == 'ö' => 'o'
      case a if a == 'y' => 'u'
      case a             => a
    })
  end updateEnding





}
