package morph_fin.rulings

import morph_fin.rulings.*
import morph_fin.rulings.rules.Gradation
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
    'M' -> 12
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
  def getWordGradationTypeForNoun(lemma: String, gradation: Gradation): WordGradationType =
    if lemma.last == 'e' && lemma.dropRight(1).endsWith(gradation.strong) then WordGradationType.Straight
    else allVowelsExcepte.contains(lemma.last) match {
      case true => WordGradationType.Straight
      case false => WordGradationType.Inverted
    }

  /**
   * root = beginning + gradation (- ending)
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
  def resolveNounException(ending: String, tpe: WordGradationType, morphemes: Morphemes): Option[GradationType] =
    import WordGradationType._
    tpe match {
      case Straight if morphemes.is(Illative) =>
        Some(GradationType.Strong)
      case Straight if morphemes.is(Genitive, Plural) && ending.endsWith("in")   =>
        Some(GradationType.Strong)
      case Inverted if morphemes.is(Nominative, Singular) ||  morphemes.is(Partitive, Singular) ||  morphemes.is(Accusative, Singular) =>
        Some(GradationType.Weak)
      case Inverted if morphemes.is(Genitive, Plural) && ending.endsWith("ten") &&  !ending.endsWith("tten") =>
        Some(GradationType.Weak)
      case Inverted =>
        Some(GradationType.Strong)
      case _ => None
    }

  /**
   * The following method manages exceptions to consonant gradation for verbs.
   */
  def resolveVerbException(lemma: String, tpe: WordGradationType, morphemes: Morphemes): Option[GradationType] =
    import WordGradationType._
    tpe match {
      case Straight if morphemes.is(Indicative, Present, Passive, Positive) =>
        Some(GradationType.Weak)
      case Straight if morphemes.is(Imperative, Present, Active, SingularSecond) =>
        Some(GradationType.Weak)
      case Straight if morphemes.is(Indicative, Present, Negative) =>
        Some(GradationType.Weak)
      case Inverted if morphemes.is(Indicative, Present, Passive, Negative) =>
        Some(GradationType.Weak)
      case Inverted if morphemes.is(Indicative, Present, Active, Positive) && endsWith_tA(lemma) =>
        Some(GradationType.Strong)
      case Inverted if morphemes.is(Indicative, Passive, Positive) && endsWith_tA(lemma)  =>
        Some(GradationType.Weak)
      case Inverted if morphemes.root == AInfinitive && endsWith_tA(lemma)  =>
        Some(GradationType.Weak)
      case Inverted if morphemes.root == EInfinitive && morphemes.is(Active) && endsWith_tA(lemma)  =>
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
  def splitByGradationLocation(root: String, gradation: Gradation, ruleNumber: Int = -1, drop: Int = 0): (String, String) =
    def trySplit(amount: Int): Option[(String, String)] =
      val droppedRoot = root.dropRight(amount)
      if droppedRoot.endsWith(gradation.strong) then Some(droppedRoot.dropRight(gradation.strong.length) , root.takeRight(amount))
      else if droppedRoot.endsWith(gradation.weak) then Some(droppedRoot.dropRight(gradation.weak.length) , root.takeRight(amount))
      else None

    def recursion(amount: Int): (String, String) =
      if(amount >= 10) (root, "")
      else
        val trying = trySplit(amount)
        trying match {
          case Some(a) => a
          case None    => recursion(amount + 1)
        }

    def resolveEmptyWeakCase: (String, String) =
      if root.dropRight(0).endsWith(gradation.strong) then (root.dropRight(0).dropRight(gradation.strong.length) , root.takeRight(0))
      else if root.dropRight(1).endsWith(gradation.strong) then (root.dropRight(1).dropRight(gradation.strong.length) , root.takeRight(1))
      else if drop > 0 then (root.dropRight(0), root.takeRight(0))
      else if Letters.isConsonant(root.last) then (root.dropRight(2), root.takeRight(2))
      else (root.dropRight(1), root.takeRight(1))

    if ruleNumber == 28 then (root.dropRight(1), "")
    else if gradation.weak.isEmpty then resolveEmptyWeakCase
    else recursion(0)

}
