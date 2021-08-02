package morph_fin.rulings

import morph_fin.rulings.Case.Illative
import morph_fin.rulings.Form.Singular
import morph_fin.rulings.nomines.Gradation
import morph_fin.utils.{Hyphenation, Letters}

//Gradation

enum GradationType:
  case Strong, Weak

enum WordGradationType:
  case Straight, Inverted

object GradationHandler {
  val allVowelsExcepte = Seq('a', 'o', 'u', 'i', 'ä', 'ö', 'y')

  def getWordGradationType(lemma: String, gradation: Gradation): WordGradationType =
    ???

  /**
   * ending is defined from the word by:
   * root | gradation | ending
   */
  def gradationTypeByEnding(ending: String): GradationType =
    assert(ending.nonEmpty)
    val firstSyllable = Hyphenation.getForEnding(ending).head
    Letters.isConsonant(firstSyllable.last) match {
      case true  => GradationType.Weak
      case false => GradationType.Strong
    }

  /**
   * In straight consonant gradation illative has strong gradation.
   * In inverted consonant gradation words ending with 'e' can have strong gradation if the following syllabus contains two vowels.
   * Implementation for inverted exceptions does not follow this logic, but will give the same output.
   */
  def resolveNomineException(lemma: String, tpe: WordGradationType, morphemes: NomineMorphemes): Option[GradationType] =
    import Case._
    import Form._
    import WordGradationType._
    (tpe, morphemes) match {
      case (Straight, NomineMorphemes(Illative, Singular | Plural)) => Some(GradationType.Strong)
      case (Inverted, NomineMorphemes(Nominative | Partitive | Akkusative, Plural)) if lemma.last == 'e' => Some(GradationType.Weak)
      case (Inverted, _) if lemma.last == 'e' => Some(GradationType.Strong)
      case _ => None
    }

  /**
   *
   *
   */
  def resolveVerbException(lemma: String, tpe: WordGradationType, morphemes: VerbMophemes): Option[GradationType] =
    import WordGradationType._
    import Voice._, Modus._, Tempus._,  Mode._
    (tpe, morphemes) match {
      case (Straight, VerbMophemes.Standard(Indicative, Presens, Persona.Passive, Positive)) =>
        Some(GradationType.Weak)
      case (Straight, VerbMophemes.Standard(Imperative, Presens, Persona.Active(Singular, PersonaNumber.Second), _)) =>
        Some(GradationType.Weak)
      case (Straight, VerbMophemes.Standard(Indicative, Presens, _, Negative)) =>
        Some(GradationType.Weak)
    }



  /**
   * @param root - root of all nomine bendings.
   * Example:
   * splitByGradationLocation("tanko", nk-ng) = (ta, o)
   */
  def splitByGradationLocation(root: String, gradation: Gradation): (String, String) =
    def trySplit(amount: Int): Option[(String, String)] =
      val droppedRoot = root.dropRight(amount)
      if droppedRoot.endsWith(gradation.strong) then Some(droppedRoot.dropRight(gradation.strong.length) , root.takeRight(amount))
      else if droppedRoot.endsWith(gradation.weak) then Some(droppedRoot.dropRight(gradation.weak.length) , root.takeRight(amount))
      else None

    def recursion(amount: Int): (String, String) =
      if(amount >= 3) (root, "")
      else
        val trying = trySplit(amount)
        trying match {
          case Some(a) => a
          case None    => recursion(amount + 1)
        }

    def resolveEmptyWeakCase: (String, String) =
      if root.dropRight(0).endsWith(gradation.strong) then (root.dropRight(0).dropRight(gradation.strong.length) , root.takeRight(0))
      else if root.dropRight(1).endsWith(gradation.strong) then (root.dropRight(1).dropRight(gradation.strong.length) , root.takeRight(1))
      else if !Letters.isVowel(root.last) then  (root.dropRight(2), root.takeRight(2))
      else (root.dropRight(1), root.takeRight(1))

    if gradation.weak.isEmpty then resolveEmptyWeakCase
    else recursion(0)

}
