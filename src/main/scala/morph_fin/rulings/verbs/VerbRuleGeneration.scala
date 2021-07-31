package morph_fin.rulings.verbs

import morph_fin.rulings._
import morph_fin.rulings.nomines.{Gradation, LongestStartingSubstring, NomineBending, NomineEnding, NomineExampleBending}


case class VerbEnding(val morphemes: VerbMophemes, val ending: String)
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
      val mappedEndings = VerbMappings.list.map(tuple => VerbEnding(tuple._1, cases.find(value => value.morphemes == tuple._2).getOrElse(throw new Exception(s"${tuple._2}")).ending))
      bending.copy(cases = cases ++ mappedEndings)

    def resolveGradation(exampleBendind: VerbExampleBendind, gradation: Gradation): VerbBending =
      //Resolve root. Resolvation is based on nomine_rules.txt file analysation
      val root = exampleBendind.lemma.dropRight(2)

      //Resolve cases
      val endings = exampleBendind.cases.map(resolveEnding(_, root, gradation))
      VerbBending(exampleBendind.number, 2, true, endings)

    /**
     *  Removes root and gradation from cased word to resolve ending.
     *  Example: soudamme -> sou | d | amme -> -amme
     */
    def resolveEnding(tuple: (VerbMophemes, String), root: String, gradation: Gradation): VerbEnding =
      import GradationType.*
      val endingWithGradation = tuple._2.drop(root.length)
      val ending = if endingWithGradation.startsWith(gradation.strong) then endingWithGradation.drop(gradation.strong.length)
      else endingWithGradation.drop(gradation.weak.length)
      VerbEnding(tuple._1, ending)

    def resolveNonGradation(ruling: VerbExampleBendind): VerbBending =
      val words = ruling.cases.map(_._2)
      val root = LongestStartingSubstring(words)
      val drop = ruling.lemma.length - root.length
      val cases = ruling.cases.map(a => VerbEnding(a._1, a._2.drop(root.length)))
      VerbBending(ruling.number, drop, false, cases)
}
