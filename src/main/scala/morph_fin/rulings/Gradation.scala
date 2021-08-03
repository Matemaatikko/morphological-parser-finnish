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

  /**
   * Some words ending with 'e' have straight gradation. Examples: nukke, jeppe, bourette
   */
  def getWordGradationTypeForNomine(lemma: String, gradation: Gradation): WordGradationType =
    if lemma.last == 'e' && lemma.dropRight(1).endsWith(gradation.strong) then WordGradationType.Straight
    else allVowelsExcepte.contains(lemma.last) match {
      case true => WordGradationType.Straight
      case false => WordGradationType.Inverted
    }

  def getWordGradationTypeForVerb(lemma: String, gradation: Gradation): WordGradationType =
    def tryFind(amount: Int): Option[WordGradationType] =
      val dropped = lemma.dropRight(amount)
      if dropped.endsWith(gradation.strong) then Some(WordGradationType.Straight)
      else None

    def recursion(amount: Int): WordGradationType =
      if(amount >= 4) WordGradationType.Inverted
      else
        val trying = tryFind(amount)
        trying match {
          case Some(a) => a
          case None    => recursion(amount + 1)
        }

    recursion(0)
  end getWordGradationTypeForVerb

  /**
   * ending is defined from the word by:
   * root | gradation | ending
   */
  def getGradationTypeByEnding(ending: String): GradationType =
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
  def resolveNomineException(lemma: String, ending: String, tpe: WordGradationType, morphemes: NomineMorphemes): Option[GradationType] =
    import Case._
    import Form._
    import WordGradationType._
    (tpe, morphemes) match {
      case (Straight, NomineMorphemes(Illative, Singular | Plural)) =>
        Some(GradationType.Strong)
      case (Straight, NomineMorphemes(Genetive, Plural)) if ending.endsWith("in")   =>
        Some(GradationType.Strong)
      case (Inverted, NomineMorphemes(Nominative | Partitive | Akkusative, Singular)) =>
        Some(GradationType.Weak)
      case (Inverted, NomineMorphemes(Genetive, Plural)) if ending.endsWith("ten") =>
        Some(GradationType.Weak)
      case (Inverted, _)  =>
        Some(GradationType.Strong)
      case _ => None
    }

  /**
   * The following method manages exceptions to consonant gradation for verbs.
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
      case (Inverted, VerbMophemes.Standard(Indicative, Presens, Persona.Passive, Negative)) =>
        Some(GradationType.Weak)
      case (Inverted, VerbMophemes.Standard(Indicative, Presens, Persona.Active(_,_), Positive)) if endsWith_tA(lemma) =>
        Some(GradationType.Strong)
      case (Inverted, VerbMophemes.Standard(Indicative, _, Persona.Passive, Positive)) if endsWith_tA(lemma)  =>
        Some(GradationType.Weak)
      case (Inverted, VerbMophemes.InfinitiveI(_)) if endsWith_tA(lemma)  =>
        Some(GradationType.Weak)
      case (Inverted, VerbMophemes.InfinitiveII(_, Voice.Active)) if endsWith_tA(lemma)  =>
        Some(GradationType.Weak)
      case _ => None
    }

  inline def endsWith_tA(lemma: String): Boolean = (lemma.last == 'a' || lemma.last == 'ä') && lemma.dropRight(1).last == 't'


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
