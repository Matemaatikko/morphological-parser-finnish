package morph_fin.rulings

import morph_fin.rulings.Case.Illative
import morph_fin.rulings.GNumber.Singular
import morph_fin.rulings.nomines.Gradation
import morph_fin.utils.{Hyphenation, Letters}

//Gradation

enum GradationType:
  case Strong, Weak

enum WordGradationType:
  case Straight, Inverted

object GradationHandler {
  private val allVowelsExcepte = Seq('a', 'o', 'u', 'i', 'ä', 'ö', 'y')
  private val letterMap = Map(
    'A' -> 0, 'B' -> 1, 'C' -> 2, 'D' -> 3,
    'E' -> 4, 'F' -> 5, 'G' -> 6, 'H' -> 7,
    'I' -> 8, 'J' -> 9, 'K' -> 10, 'L' -> 11,
    'M' -> 12, 'N' -> 13
  )

  private val gradationMap = Seq(
    "kk" -> "k",
    "pp" -> "p",
    "tt" -> "t",
    "k" -> "",
    "p" -> "v",
    "t" -> "d",
    "nk" -> "ng",
    "mp" -> "mm",
    "lt" -> "ll",
    "nt" -> "nn",
    "rt" -> "rr",
    "k" -> "j",
    "k" -> "v"
  )

  def getGradationByLetter(letter: Char): Gradation =
    val (strong, weak) = gradationMap(letterMap(letter))
    Gradation(strong, weak)


  /**
   * Some words ending with 'e' have straight gradation. Examples: nukke, jeppe, bourette
   */
  def getWordGradationTypeForNomine(lemma: String, gradation: Gradation): WordGradationType =
    if lemma.last == 'e' && lemma.dropRight(1).endsWith(gradation.strong) then WordGradationType.Straight
    else allVowelsExcepte.contains(lemma.last) match {
      case true => WordGradationType.Straight
      case false => WordGradationType.Inverted
    }

  /**
   * root = beginnig + gradation (- ending)
   */
  def getWordGradationTypeForVerb(lemma: String, gradation: Gradation): WordGradationType =
    val root = if endsWith_tA(lemma) then lemma.dropRight(2) else lemma

    def tryFind(amount: Int): Option[WordGradationType] =
      val dropped = root.dropRight(amount)
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
    import GNumber._
    import WordGradationType._
    (tpe, morphemes) match {
      case (Straight, NomineMorphemes(Illative, Singular | Plural, _)) =>
        Some(GradationType.Strong)
      case (Straight, NomineMorphemes(Genitive, Plural, _)) if ending.endsWith("in")   =>
        Some(GradationType.Strong)
      case (Inverted, NomineMorphemes(Nominative | Partitive | Accusative, Singular, _)) =>
        Some(GradationType.Weak)
      case (Inverted, NomineMorphemes(Genitive, Plural, _)) if ending.endsWith("ten") =>
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
      case (Straight, VerbMophemes.Standard(Indicative, Present, Persona.Passive, Positive)) =>
        Some(GradationType.Weak)
      case (Straight, VerbMophemes.Standard(Imperative, Present, Persona.Active(Singular, Person.Second), _)) =>
        Some(GradationType.Weak)
      case (Straight, VerbMophemes.Standard(Indicative, Present, _, Negative)) =>
        Some(GradationType.Weak)
      case (Inverted, VerbMophemes.Standard(Indicative, Present, Persona.Passive, Negative)) =>
        Some(GradationType.Weak)
      case (Inverted, VerbMophemes.Standard(Indicative, Present, Persona.Active(_,_), Positive)) if endsWith_tA(lemma) =>
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
   * splitByGradationLocation("ies", k-_) = (i, es)
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
      else if !Letters.isVowel(root.last) then  (root.dropRight(1), root.takeRight(1))
      else (root.dropRight(0), root.takeRight(0))

    if gradation.weak.isEmpty then resolveEmptyWeakCase
    else recursion(0)

}
