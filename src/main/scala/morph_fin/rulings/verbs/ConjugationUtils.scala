package morph_fin.rulings.verbs

import morph_fin.rulings.nouns.*
import morph_fin.rulings.*
import morph_fin.rulings.GradationHandler
import morph_fin.rulings.morpheme.{AInfinitive, Active, EInfinitive, Finite, Indicative, Inessive, InfinitiveIV, InfinitiveV, Morphemes, Positive, Present, SingularThird}
import morph_fin.rulings.rules.*
import morph_fin.utils.Letters
import morph_fin.utils.VocalizationUtils.{resolveVocalization, updateVocalization}

object ConjugationUtils {

  def generateConjugations(word: Word)(using rules: Seq[ConjugationRule]): Seq[InflectedWord] =
    val rule = rules.find(_.ruleNumber == word.ruleNumber).getOrElse(throw new Exception(s"No verb rule found for: ${word.ruleNumber}"))
    //Resolve root
    val root = word.lemma.dropRight(rule.drop)
    //Resolve gradation
    val rootDividedByGradation = word.gradationOpt match {
      case Some(gradation)  => GradationHandler.splitByGradationLocation(root, gradation, rule.ruleNumber)
      case _                => (root, "")
    }
    rule.cases.map(ending => resolveWord(ending, rootDividedByGradation, word, rule))
  end generateConjugations

  def resolveWord(ending: Conjugation, root: (String, String), word: Word, rule: ConjugationRule): InflectedWord =
    val updatedEnding = updateEnding(ending, word.lemma, rule)
    val gradation = word.gradationOpt match {
      case Some(gradation) if ending.tpe == VerbGradationType.Strong => gradation.strong
      case Some(gradation) if ending.tpe == VerbGradationType.Weak => gradation.weakValue
      case Some(gradation) =>
        val wordGradationType = GradationHandler.getWordGradationTypeForVerb(word.lemma, gradation)
        val defaultGradationType = GradationHandler.getGradationTypeByEnding(root._2 + updatedEnding)
        val exceptionGradationType = GradationHandler.resolveVerbException(word.lemma, wordGradationType, ending.morphemes)
        val tpe = exceptionGradationType.getOrElse(defaultGradationType)
        if(tpe == GradationType.Strong) gradation.strong else gradation.weakValue
      case None            => ""
    }
    val updatedGradation = if ending.tpe == VerbGradationType.TSGradation then gradation.dropRight(1) + "s" else gradation
    val structuredWord = StructuredWord(root._1, updatedGradation, root._2 + updatedEnding)
    val resultWord = handleSpecialCase(structuredWord, word.ruleNumber, ending.morphemes)
    InflectedWord(resultWord, ending.morphemes, word.lemma)
  end resolveWord
  
  val IndPreS3 = Finite ~ Indicative ~ Present ~ Active ~ SingularThird ~ Positive

  def handleSpecialCase(word: StructuredWord, ruleNumber: Int, morphemes: Morphemes): StructuredWord =
    if morphemes == IndPreS3 && word.ending.nonEmpty && ruleNumber != 64 then
      val dropped = word.ending.dropRight(1)
      val char = dropped.lastOption.getOrElse(word.root.last)
      word.copy(ending = dropped + char)
    else word


  def updateEnding(ending: Conjugation, lemma: String, rule: ConjugationRule): String =
    val AInfEnding = rule.cases.find(_.morphemes.is(AInfinitive)).get.ending
    val replacementMap = Replacement.resolveMap(lemma, AInfEnding)

    val result = ending.ending.map(_ match {
      case RepChar.Rep(c) => replacementMap(c).toString
      case RepChar.Ch(c)  => c.toString
    }).mkString("")

    updateVocalization(result, resolveVocalization(lemma))
  end updateEnding
}
