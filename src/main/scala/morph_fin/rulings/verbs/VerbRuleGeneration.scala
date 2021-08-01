package morph_fin.rulings.verbs

import morph_fin.rulings._
import morph_fin.rulings.nomines.{Gradation, LongestStartingSubstring, NomineBending, NomineEnding, NomineExampleBending}


enum VerbGradationType:
  case Strong, Weak, Nothing, Missing

case class VerbEnding(morphemes: VerbMophemes, ending: String, tpe: VerbGradationType)
case class VerbBending(ruleNumber: Int, drop: Int, isGradation: Boolean, cases: Seq[VerbEnding])

extension (bending: VerbBending)
  def findCase(morphemes: VerbMophemes): VerbEnding =
    bending.cases.find(ending => ending.morphemes == morphemes).getOrElse(throw new Error("Non-comprehensive matching in rules"))

object GenerateVerbRules {

    def apply(exampleBendind: VerbExampleBendind): VerbBending =
      val result = exampleBendind.gradation match {
        case Some(gradation) => resolveGradation(exampleBendind, gradation)
        case None            => resolveNonGradation(exampleBendind)
      }
      if result.ruleNumber == 77 || result.ruleNumber == 78 then result else  addMappedBendings(result)

    def addMappedBendings(bending: VerbBending): VerbBending =
      val cases = bending.cases
      val mappedEndings = VerbMappings.list.map(tuple => VerbEnding(tuple._1, cases.find(value => value.morphemes == tuple._2).get.ending,  VerbGradationType.Nothing))
      bending.copy(cases = cases ++ mappedEndings)

    def resolveGradation(exampleBendind: VerbExampleBendind, gradation: Gradation): VerbBending =
      //Resolve root. Resolvation is based on verb_rules.txt file analysation. Root does not contain gradation part.
      val drop = exampleBendind.number match {
        case 54 | 55 | 60 | 76 => 3
        case 57 | 59 => 4
      }
      val root = exampleBendind.lemma.dropRight(drop)

      //Resolve cases
      val endings = exampleBendind.cases.map(resolveEnding(_, root, gradation))
      VerbBending(exampleBendind.number, drop, true, endings)

    /**
     *  Removes root and gradation from cased word to resolve ending.
     *  Example: soudamme -> sou | d | amme -> -amme
     *
     *  Note: Some verbs with gradation have end of their gradation replaced by s in Active-Imperfect
     */
    def resolveEnding(tuple: (VerbMophemes, String), root: String, gradation: Gradation): VerbEnding =
      import GradationType.*
      val endingWithGradation = tuple._2.drop(root.length)
      val (ending, tpe) = if endingWithGradation.startsWith(gradation.strong) then endingWithGradation.drop(gradation.strong.length) -> VerbGradationType.Strong
      else if endingWithGradation.startsWith(gradation.weak) then endingWithGradation.drop(gradation.weak.length) ->  VerbGradationType.Weak
      else endingWithGradation ->  VerbGradationType.Missing
      VerbEnding(tuple._1, ending, tpe)

    def resolveNonGradation(ruling: VerbExampleBendind): VerbBending =
      val words = ruling.cases.map(_._2)
      val root = LongestStartingSubstring(words)
      val drop = ruling.lemma.length - root.length
      val cases = ruling.cases.map(a => VerbEnding(a._1, a._2.drop(root.length), VerbGradationType.Nothing))
      VerbBending(ruling.number, drop, false, cases)
}
