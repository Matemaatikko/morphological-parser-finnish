package morph_fin.rulings.verbs

import morph_fin.rulings.NomineMorphemes
import morph_fin.rulings.nomines.*
import morph_fin.rulings.*
import morph_fin.rulings.GradationHandler
import morph_fin.utils.Letters
import morph_fin.utils.VocalizationUtils.{resolveVocalization, updateVocalization}

object ConjugationUtils {

  def addConjugations(rules: Seq[ConjugationRules], word: Word): Seq[ResultWord] =
    val rule = rules.find(_.ruleNumber == word.ruleNumber).getOrElse(throw new Exception(s"No verb rule found for: ${word.ruleNumber}"))
    //Resolve root
    val root = word.lemma.dropRight(rule.drop)
    //Resolve gradation
    val rootDividedByGradation = word.gradation match {
      case Some(gradation)  => GradationHandler.splitByGradationLocation(root, gradation)
      case _                => (root, "")
    }
    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, word, rule))
  end addConjugations

  def resolveWord(ending: Conjugation, root: (String, String), word: Word, rule: ConjugationRules): ResultWord =
    val updatedEnding = updateEnding(ending, word.lemma, rule)
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
    val updatedGradation = if ending.tpe == VerbGradationType.Missing then gradation.dropRight(1) else gradation
    val structuredWord = StructuredWord(root._1, updatedGradation, root._2 + updatedEnding)
    val resultWord = handleSpecialCase(structuredWord, word.ruleNumber, ending.morphemes)
    ResultWord(resultWord, ending.morphemes, word.lemma)
  end resolveWord
  
  val IndPreS3 = VerbMophemes.Standard(Modus.Indicative, Tempus.Present, Persona.Active(GNumber.Singular, Person.Third), Mode.Positive)

  def handleSpecialCase(word: StructuredWord, ruleNumber: Int, morphemes: Morphemes): StructuredWord =
    if morphemes == IndPreS3 && word.ending.nonEmpty && ruleNumber != 64 then
      val dropped = word.ending.dropRight(1)
      val char = dropped.lastOption.getOrElse(word.root.last)
      word.copy(ending = dropped + char)
    else word


  def updateEnding(ending: Conjugation, lemma: String, rule: ConjugationRules): String =
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