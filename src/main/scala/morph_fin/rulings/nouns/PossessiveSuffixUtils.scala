package morph_fin.rulings.nouns

import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Accusative, Genitive, Illative, Morphemes, Nominative, PossessiveSuffix, Singular, Translative}
import morph_fin.rulings.rules.Gradation
import morph_fin.utils.Letters

case class PSuffix(suffixType: PossessiveSuffix, ending: String)


object PossessiveSuffixUtils {

  val suffixes = Seq(
    PSuffix(PossessiveSuffix.SingularFirst, "ni"),
    PSuffix(PossessiveSuffix.SingularSecond, "si"),
    PSuffix(PossessiveSuffix.PluralFirst, "mme"),
    PSuffix(PossessiveSuffix.PluralSecond, "nne"),
    PSuffix(PossessiveSuffix.ThirdPos, "nsa")
  )


  def isSuitableForSuffix(morphemes: Morphemes) =
    morphemes.isNot(Nominative, Singular) && morphemes.isNot(Accusative)


  /**
 * Note: Nominative, Singular is skipped due to its similarity to Genitive, Singular.
 */
  def onlySuffixes(inflectedWord: InflectedWord): Seq[InflectedWord] =
    import inflectedWord._
    val suffixBody = getRootForNonVnSuffixes(inflectedWord, gradationOpt)
    val VnSuffix = addVnSuffix(suffixBody, gradationOpt)

    if(!isSuitableForSuffix(morphemes)) Nil
    else {
      suffixes.map(suffix =>
        val finalEnding = updateCorrectVowelsToEnding(suffixBody.word.ending + suffix.ending, inflectedWord.lemma.toString)
        val strWord = suffixBody.word.copy(ending = finalEnding)
          InflectedWord(strWord, morphemes ~ suffix.suffixType, suffixBody.lemma)
      ) ++ VnSuffix
    }
  end onlySuffixes

  /**
   * Note: Nominative, Singular is skipped due to its similarity to Genitive, Singular.
   */
  def addSuffixes(inflectedWord: InflectedWord): Seq[InflectedWord] =
    import inflectedWord._
    val suffixBody = getRootForNonVnSuffixes(inflectedWord, gradationOpt)
    val VnSuffix = addVnSuffix(suffixBody, gradationOpt)

    if(!isSuitableForSuffix(morphemes)) Seq(inflectedWord)
    else {
      Seq(inflectedWord) ++ suffixes.map(suffix =>
        val finalEnding = updateCorrectVowelsToEnding(suffixBody.word.ending + suffix.ending, inflectedWord.lemma.toString)
        val strWord = suffixBody.word.copy(ending = finalEnding)
        InflectedWord(strWord, morphemes ~ suffix.suffixType, suffixBody.lemma)
      ) ++ VnSuffix
    }
  end addSuffixes

  inline def getRootForNonVnSuffixes(suffixBody: InflectedWord, gradationOpt: Option[Gradation]): InflectedWord =
    val root = suffixBody.word.root
    val updatedGradation = updateGradation(suffixBody.word.gradation, suffixBody.morphemes, gradationOpt)
    val updatedEnding = handle_mm_case(remove_t_and_n_fromEnding(suffixBody.word.ending))
    val updatedEnding2 = handleDECase(handleTranslativeCase(updatedEnding, suffixBody.morphemes))
    suffixBody.copy(word = StructuredWord(root, updatedGradation, updatedEnding2))

  /**
   * If possessive suffix is not allowed, then Nil is returned.
   */
  inline def addVnSuffix(suffixBody: InflectedWord, gradationOpt: Option[Gradation]): Seq[InflectedWord] =
    val root = getRootForNonVnSuffixes(suffixBody, gradationOpt)
    resolveSecondUsageOfThirdPerson(root)

  //========================================================================

  /**
   * If word ends with same two vowels, then skipped. Example: aapa, aapaa'an
   */
  private inline def resolveSecondUsageOfThirdPerson(inflectedWord: InflectedWord): Seq[InflectedWord] =
    import inflectedWord._
    val additionCondition = morphemes match {
      case _ if morphemes.is(Nominative) || morphemes.is(Genitive) || morphemes.is(Illative) => false
      case _ if Letters.isVowel(word.last) && word.last == (word.toString).dropRight(1).last => false
      case _ => true
    }
    if additionCondition
    then Seq(inflectedWord.copy(
        word = word.copy(ending = word.ending + word.last + "n"),
        morphemes = morphemes ~ PossessiveSuffix.ThirdPos
      ))
    else Nil

  private inline def updateGradation(default: String, morphemes: Morphemes, gradationOpt: Option[Gradation]): String =
    //Gradation
    val strongGradationCondition = morphemes.is(Nominative) || morphemes.is(Genitive, Singular)
    gradationOpt match {
      case Some(gradation) if strongGradationCondition => gradation.strong
      case   _ => default
    }

  private inline def remove_t_and_n_fromEnding(ending: String): String =
    if ending.lastOption == Some('t') || ending.lastOption == Some('n')
    then ending.dropRight(1) else ending

  //Example: kauniimma(n) -> kauniimpa(ni)
  private inline def handle_mm_case(ending: String): String =
    if ending.endsWith("mma") then ending.dropRight(3) + "mpa" else ending

  //Example: kahde(n) -> kahte(ni)
  private inline def handleDECase(ending: String): String =
    if ending.endsWith("de") then ending.dropRight(2) + "te" else ending


  //kauniiksi -> kauniikseni
  private inline def handleTranslativeCase(ending: String, morphemes: Morphemes): String =
    if morphemes.is(Translative) then ending.dropRight(1) + "e"
    else ending

  import morph_fin.utils.VocalizationUtils._

  private inline def updateCorrectVowelsToEnding(ending: String, word: String): String =
    val vocalization = resolveVocalization(word)
    updateVocalization(ending, vocalization)
}
