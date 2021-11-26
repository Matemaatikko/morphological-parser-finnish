package morph_fin.rulings.rules

import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{AInfinitive, Morphemes}
import morph_fin.utils.LongestStartingSubstring
import morph_fin.rulings.rules.GenerateDeclensionRules.splitByFirstConsonant
import morph_fin.rulings.rules.{Gradation, RepChar}
import morph_fin.utils.Letters

enum VerbGradationType:
  case Strong, Weak, Nothing, TSGradation

case class Conjugation(morphemes: Morphemes, ending: Seq[RepChar], tpe: VerbGradationType)
case class ConjugationRule(
                             ruleNumber: Int,
                             drop: Int,
                             isGradation: Boolean,
                             replacementVowels: Seq[Char],
                             cases: Seq[Conjugation])


object GenerateConjugationRules {

    def apply(exampleConjugations: VerbExampleConjugations): ConjugationRule =
      exampleConjugations.gradation match {
        case Some(gradation) => resolveGradation(exampleConjugations, gradation)
        case None            => resolveNonGradation(exampleConjugations)
      }

    def resolveGradation(exampleConjugations: VerbExampleConjugations, gradation: Gradation): ConjugationRule =
      //Resolve root. Resolvation is based on verb_rules.txt file analysation. Root does not contain gradation part.
      val dropForRoot = exampleConjugations.number match {
        case 54 | 55 | 60 | 76 => 3
        case 57 | 59 => 4
      }
      val dropForBending = 2
      val root = exampleConjugations.lemma.dropRight(dropForRoot)

      //Resolve cases
      val endings = exampleConjugations.cases.map(resolveEnding(_, root, gradation))
      ConjugationRule(exampleConjugations.number, dropForBending, true, Nil, endings)


    import morph_fin.rulings.rules.RepChar.*
    /**
     *  Removes root and gradation from cased word to resolve ending.
     *  Example: soudamme -> sou | d | amme -> -amme
     *
     *  Note: Some verbs with gradation have end of their gradation replaced by s in Active-Imperfect
     */
    def resolveEnding(tuple: (Morphemes, String), root: String, gradation: Gradation): Conjugation =
      import GradationType.*
      val endingWithGradation = tuple._2.drop(root.length)

      val (ending, tpe) = if endingWithGradation.startsWith(gradation.strong)
        then endingWithGradation.drop(gradation.strong.length) -> VerbGradationType.Strong
      else if endingWithGradation.startsWith(gradation.weak)
        then endingWithGradation.drop(gradation.weak.length) -> VerbGradationType.Weak
      else if gradation.strong.endsWith("t") && endingWithGradation.startsWith(gradation.strong.dropRight(1) + "s") //t-s gradation
        then endingWithGradation.drop(gradation.strong.length) -> VerbGradationType.TSGradation
      else throw new Exception("Failed to resolve gradation: " + root + ", " + endingWithGradation)

      Conjugation(tuple._1, ending.map(Ch(_)), tpe)

    def resolveNonGradation(exampleConjugations: VerbExampleConjugations): ConjugationRule =
      val words = exampleConjugations.cases.map(_._2)
      val root = LongestStartingSubstring(words)
      val drop = exampleConjugations.lemma.length - root.length

      val infinitive = exampleConjugations.cases.find(_._1.root == AInfinitive).get
      val replacementVowels = infinitive._2.drop(root.length).takeWhile(Letters.isVowel(_))
      val cases = exampleConjugations.cases.map(a => resolveNonGradationCase(a._1, a._2, root, replacementVowels))
      ConjugationRule(exampleConjugations.number, drop, false, replacementVowels, cases)

    def resolveNonGradationCase(morphemes: Morphemes, word: String, root: String, replacementVowels: Seq[Char]): Conjugation =
      val ending = word.drop(root.length)
      val (start, end) = splitByFirstConsonant(ending)
      val resultEnding = start.map(c => if(replacementVowels.contains(c)) then Rep(c) else Ch(c)) ++ end.map(Ch(_))
      Conjugation(morphemes, resultEnding, VerbGradationType.Nothing)
}
