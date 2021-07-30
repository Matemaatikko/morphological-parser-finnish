package morph_fin.rulings

import morph_fin.rulings.nomines.Gradation

object GradationHandler {
  val allVowelsExcepte = Seq('a', 'o', 'u', 'i', 'ä', 'ö', 'y')
  val listOfAllVowels = Seq('a', 'o', 'i', 'e', 'u', 'y', 'ä', 'ö')

  /**
   * Method gives gradation type for bending based on last letter of lemma.
   */
  def resolveGradationType(lastLetterOfLemma: Char, morphemes: NomineMorphemes): GradationType =
    import Case._
    import Form._

    if allVowelsExcepte.contains(lastLetterOfLemma) then
      morphemes match {
        case NomineMorphemes(Nominative, Singular)  => GradationType.Strong
        case NomineMorphemes(Partitive, Singular)   => GradationType.Strong
        case NomineMorphemes(Akkusative, Singular)  => GradationType.Strong
        case NomineMorphemes(Illative, Singular)    => GradationType.Strong
        case NomineMorphemes(Essive, Singular)      => GradationType.Strong
        case NomineMorphemes(_, Singular)           => GradationType.Weak

        case NomineMorphemes(Partitive, Plural)   => GradationType.Strong
        case NomineMorphemes(Genetive, Plural)    => GradationType.Strong
        case NomineMorphemes(Illative, Plural)    => GradationType.Strong
        case NomineMorphemes(Essive, Plural)      => GradationType.Strong
        case NomineMorphemes(Komitative, Plural)  => GradationType.Strong
        case NomineMorphemes(_, Plural)           => GradationType.Weak
      }
    else
      morphemes match {
        case NomineMorphemes(Nominative, Singular)  => GradationType.Weak
        case NomineMorphemes(Akkusative, Singular)  => GradationType.Weak
        case NomineMorphemes(Partitive, Singular)   => GradationType.Weak
        case NomineMorphemes(_, Singular)           => GradationType.Strong
        case NomineMorphemes(_, Plural)             => GradationType.Strong
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
      else if !listOfAllVowels.contains(root.last) then  (root.dropRight(2), root.takeRight(2))
      else (root.dropRight(1), root.takeRight(1))

    if gradation.weak.isEmpty then resolveEmptyWeakCase
    else recursion(0)

}
