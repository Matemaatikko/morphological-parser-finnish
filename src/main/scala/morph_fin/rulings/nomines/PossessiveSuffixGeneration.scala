package morph_fin.rulings.nomines

import morph_fin.rulings.PossessiveSuffix.{PluralFirst, PluralSecond, SingularFirst, SingularSecond, Third}
import morph_fin.rulings.{Case, GNumber, NomineMorphemes, PossessiveSuffix}
import morph_fin.rulings.nomines.GenerateDeclensionWords.{resolveVocalization, updateVocalization}
import morph_fin.utils.Letters

object PossessiveSuffixGeneration {

  val suffixes = Seq(
    PSuffix(SingularFirst, "ni"),
    PSuffix(SingularSecond, "si"),
    PSuffix(PluralFirst, "mme"),
    PSuffix(PluralSecond, "nne"),
    PSuffix(Third, "nsa")
  )

  /**
   * Note: Case.Nominative, GNumber.Singular is skipped due to its similarity to Case.Genitive, GNumber.Singular.
   */
  def addSuffixes(resultWord: ResultWord, gradation: Option[Gradation]): Seq[ResultWord] =
    val root = resultWord.word.root
    val morphemes = resultWord.morphemes.asInstanceOf[NomineMorphemes]
    val updatedGradation = updateGradation(resultWord.word.gradation, morphemes, gradation)
    val updatedEnding = remove_t_and_n_fromEnding(resultWord.word.ending)
    val additional = resolveSecondUsageOfThirdPerson(morphemes, resultWord)(root, updatedGradation, updatedEnding)

    if(morphemes == NomineMorphemes(Case.Nominative, GNumber.Singular)) Seq(resultWord)
    else {
      Seq(resultWord)
        ++ suffixes.map(suffix =>
        val finalEnding = updateCorrectVowelsToEnding(updatedEnding + suffix.ending, resultWord.lemma.toString)
        resultWord.copy(
          word = StructuredWord(root, updatedGradation, finalEnding),
          morphemes = morphemes.copy(suffixOpt = Some(suffix.info))
        )) ++ additional
    }
  end addSuffixes


  /**
   * If word ends with two same vowels, then skipped. Example: aapa, aapaa'an
   */
  inline def resolveSecondUsageOfThirdPerson(morphemes: NomineMorphemes, resultWord: ResultWord)
                                     (root: String, gradation: String, ending: String): Seq[ResultWord] =
    val additionCondition = resultWord.morphemes match {
      case NomineMorphemes(Case.Nominative, _, _) => false
      case NomineMorphemes(Case.Genitive, _, _) => false
      case NomineMorphemes(Case.Illative, _, _) => false
      case _ if Letters.isVowel(ending.last) && ending.last == (root + gradation + ending).dropRight(1).last => false
      case _ => true
    }
    if additionCondition
    then Seq(resultWord.copy(
        word = StructuredWord(root, gradation, ending + ending.last + "n"),
        morphemes = morphemes.copy(suffixOpt = Some(PossessiveSuffix.Third))
      ))
    else Nil

  inline def updateGradation(default: String, morphemes: NomineMorphemes, gradationOpt: Option[Gradation]): String =
    //Gradation
    val strongGradationCondition =
      morphemes match {
        case NomineMorphemes(Case.Nominative, _, _) => true
        case NomineMorphemes(Case.Genitive, GNumber.Singular, _) => true
        case _ => false
      }
    gradationOpt match {
      case Some(gradation) if strongGradationCondition => gradation.strong
      case   _ => default
    }

  inline def remove_t_and_n_fromEnding(ending: String): String =
    if ending.lastOption == Some('t') || ending.lastOption == Some('n')
    then ending.dropRight(1) else ending


  inline def updateCorrectVowelsToEnding(ending: String, word: String): String =
    val vocalization = GenerateDeclensionWords.resolveVocalization(word)
    GenerateDeclensionWords.updateVocalization(ending, vocalization)
}
