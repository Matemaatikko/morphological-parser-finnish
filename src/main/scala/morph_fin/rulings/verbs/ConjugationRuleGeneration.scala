package morph_fin.rulings.verbs

import morph_fin.rulings._
import morph_fin.rulings.nomines.{Gradation, LongestStartingSubstring, DeclensionRules, Declension, NomineExampleDeclensions}


enum VerbGradationType:
  case Strong, Weak, Nothing, Missing

case class Conjugation(morphemes: VerbMophemes, ending: String, tpe: VerbGradationType)
case class ConjugationRules(ruleNumber: Int, drop: Int, isGradation: Boolean, cases: Seq[Conjugation])

extension (rules: ConjugationRules)
  def findCase(morphemes: VerbMophemes): Conjugation =
    rules.cases.find(ending => ending.morphemes == morphemes).getOrElse(throw new Error("Non-comprehensive matching in rules"))

object GenerateConjugationRules {

    def apply(exampleConjugations: VerbExampleConjugations): ConjugationRules =
      val result = exampleConjugations.gradation match {
        case Some(gradation) => resolveGradation(exampleConjugations, gradation)
        case None            => resolveNonGradation(exampleConjugations)
      }
      if result.ruleNumber == 77 || result.ruleNumber == 78 then result else  addMappedBendings(result)

    def addMappedBendings(rules: ConjugationRules): ConjugationRules =
      val cases = rules.cases
      val mappedEndings = VerbMappings.list.map(tuple => Conjugation(tuple._1, cases.find(value => value.morphemes == tuple._2).get.ending,  VerbGradationType.Nothing))
      rules.copy(cases = cases ++ mappedEndings)

    def resolveGradation(exampleConjugations: VerbExampleConjugations, gradation: Gradation): ConjugationRules =
      //Resolve root. Resolvation is based on verb_rules.txt file analysation. Root does not contain gradation part.
      val dropForRoot = exampleConjugations.number match {
        case 54 | 55 | 60 | 76 => 3
        case 57 | 59 => 4
      }
      val dropForBending = 2
      val root = exampleConjugations.lemma.dropRight(dropForRoot)

      //Resolve cases
      val endings = exampleConjugations.cases.map(resolveEnding(_, root, gradation))
      ConjugationRules(exampleConjugations.number, dropForBending, true, endings)

    /**
     *  Removes root and gradation from cased word to resolve ending.
     *  Example: soudamme -> sou | d | amme -> -amme
     *
     *  Note: Some verbs with gradation have end of their gradation replaced by s in Active-Imperfect
     */
    def resolveEnding(tuple: (VerbMophemes, String), root: String, gradation: Gradation): Conjugation =
      import GradationType.*
      val endingWithGradation = tuple._2.drop(root.length)
      val (ending, tpe) = if endingWithGradation.startsWith(gradation.strong) then endingWithGradation.drop(gradation.strong.length) -> VerbGradationType.Strong
      else if endingWithGradation.startsWith(gradation.weak) then endingWithGradation.drop(gradation.weak.length) -> VerbGradationType.Weak
      else endingWithGradation ->  VerbGradationType.Missing
      Conjugation(tuple._1, ending, tpe)

    def resolveNonGradation(ruling: VerbExampleConjugations): ConjugationRules =
      val words = ruling.cases.map(_._2)
      val root = LongestStartingSubstring(words)
      val drop = ruling.lemma.length - root.length
      val cases = ruling.cases.map(a => Conjugation(a._1, a._2.drop(root.length), VerbGradationType.Nothing))
      ConjugationRules(ruling.number, drop, false, cases)
}
