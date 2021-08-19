package morph_fin.rulings.nomines

import morph_fin.rulings.PossessiveSuffix._
import morph_fin.rulings._
import morph_fin.utils.Letters

object PossessiveSuffixGeneration {

  val suffixes = Seq(
    PSuffix(SingularFirst, "ni"),
    PSuffix(SingularSecond, "si"),
    PSuffix(PluralFirst, "mme"),
    PSuffix(PluralSecond, "nne"),
    PSuffix(ThirdPos, "nsa")
  )

  import morph_fin.rulings.MorphemesUtils._

  def isSuitableForSuffix(morphemes: Morphemes) = morphemes match {
    case NomineMorphemes(Nominative, Singular) => false
    case NomineMorphemes(Accusative, _) => false
    case _ => true
  }

  /**
   * TODO nominemorphemes might be hidden in verbal forms. Take this in account.
   * Note: Case.Nominative, GNumber.Singular is skipped due to its similarity to Case.Genitive, GNumber.Singular.
   */
  def addSuffixes(resultWord: ResultWord, gradation: Option[Gradation]): Seq[ResultWord] =
    import resultWord._
    val suffixBody = getRootForNonVnSuffixes(resultWord, gradation)
    val VnSuffix = addVnSuffix(suffixBody, gradation)

    if(isSuitableForSuffix(morphemes)) Seq(resultWord)
    else {
      Seq(resultWord) ++ suffixes.map(suffix =>
        val finalEnding = updateCorrectVowelsToEnding(suffixBody.word.ending + suffix.ending, resultWord.lemma.toString)
        val strWord = suffixBody.word.copy(ending = finalEnding)
        ResultWord(strWord, morphemes ++ suffix.info, suffixBody.lemma)
      ) ++ VnSuffix
    }
  end addSuffixes

  inline def getRootForNonVnSuffixes(suffixBody: ResultWord, gradation: Option[Gradation]): ResultWord =
    val root = suffixBody.word.root
    val morphemes = suffixBody.morphemes.asInstanceOf[NomineMorphemes]
    val updatedGradation = updateGradation(suffixBody.word.gradation, morphemes, gradation)
    val updatedEnding = remove_t_and_n_fromEnding(suffixBody.word.ending)
    suffixBody.copy(word = StructuredWord(root, updatedGradation, updatedEnding))

  /**
   * If addition is illegal, then returns Nil.
   */
  inline def addVnSuffix(suffixBody: ResultWord, gradation: Option[Gradation]): Seq[ResultWord] =
    val root = getRootForNonVnSuffixes(suffixBody, gradation)
    resolveSecondUsageOfThirdPerson(root)

  //========================================================================

  /**
   * If word ends with two same vowels, then skipped. Example: aapa, aapaa'an
   */
  private inline def resolveSecondUsageOfThirdPerson(resultWord: ResultWord): Seq[ResultWord] =
    import resultWord._
    val additionCondition = morphemes match {
      case NomineMorphemes(Nominative, _) => false
      case NomineMorphemes(Genitive, _) => false
      case NomineMorphemes(Illative, _) => false
      case _ if Letters.isVowel(word.last) && word.last == (word.toString).dropRight(1).last => false
      case _ => true
    }
    if additionCondition
    then Seq(resultWord.copy(
        word = word.copy(ending = word.ending + word.last + "n"),
        morphemes = morphemes ++ PossessiveSuffix.ThirdPos
      ))
    else Nil

  private inline def updateGradation(default: String, morphemes: NomineMorphemes, gradationOpt: Option[Gradation]): String =
    //Gradation
    val strongGradationCondition =
      morphemes match {
        case NomineMorphemes(Nominative, _) => true
        case NomineMorphemes(Genitive, Singular) => true
        case _ => false
      }
    gradationOpt match {
      case Some(gradation) if strongGradationCondition => gradation.strong
      case   _ => default
    }

  private inline def remove_t_and_n_fromEnding(ending: String): String =
    if ending.lastOption == Some('t') || ending.lastOption == Some('n')
    then ending.dropRight(1) else ending

  import morph_fin.utils.VocalizationUtils._

  private inline def updateCorrectVowelsToEnding(ending: String, word: String): String =
    val vocalization = resolveVocalization(word)
    updateVocalization(ending, vocalization)
}
