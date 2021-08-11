package morph_fin.rulings.nomines

import morph_fin.rulings.*
import morph_fin.utils.Letters

enum NomineGradationType:
  case Strong, Weak, Nothing


enum RepChar:
  case Ch(c: Char)
  case Rep(key: Char)

import RepChar._




case class Declension(morphemes: NomineMorphemes, ending: Seq[RepChar], tpe: NomineGradationType)
case class DeclensionRules(
                            ruleNumber: Int,
                            drop: Int,
                            isGradation: Boolean,
                            replacementVowels: Seq[Char],
                            cases: Seq[Declension]
                          )

extension (bending: DeclensionRules)
  def findCase(morphemes: NomineMorphemes): Declension =
    bending.cases.find(ending => ending.morphemes == morphemes).getOrElse(throw new Error("Non-comprehensive matching in rules"))


/**
 * Idea of the following method is to find endings for each morphemes based on example declension.
 * No gradation / Example:
 *  S|Nom: sisar
 *  S|Gen: sisaren
 *  S|Par: sisarta
 *  S|Ine: sisaressa
 *  etc.
 * -----------------
 *  root = sisar
 *  drop = 0
 *  endings: -, -en, -rta, -essa, etc.
 * -----------------
 * Drop / No gradation / Example:
 * S|Nom: paperi
 * S|Gen: paperin
 * P|Ine: papereissa
 * -----------------
 * root = paper
 * drop = 1
 * endings: -i, -in, -eissa
 * -----------------
 * Gradation / Example:
 * S|Nom: solakka
 * S|Gen: solakan
 * ...
 * P|Ine: solakoissa
 * -----------------
 * root = sola + G
 * drop = 1
 * endings = -a, -an, -oissa
 */
object GenerateDeclensionRules {

  def apply(exampleDeclensions: NomineExampleDeclensions): DeclensionRules =
    exampleDeclensions.gradation match {
      case Some(gradation) => resolveGradation(exampleDeclensions, gradation)
      case None            => resolveNonGradation(exampleDeclensions)
    }

  import Declension.*

  def resolveGradation(exampleDeclensions: NomineExampleDeclensions, gradation: Gradation): DeclensionRules =
    //Resolve root. Resolvation is based on nomine_rules.txt file analysation
    val drop = if exampleDeclensions.number == 35 then 4 else 3
    val root = exampleDeclensions.lemma.dropRight(drop)

    //Resolve cases
    val cases = exampleDeclensions.cases.map(resolveCase(_, root, gradation))
    DeclensionRules(exampleDeclensions.number, drop, true, Nil, cases)

  /**
   *  Removes root and gradation from cased word to resolve ending.
   *  Example: solakat -> sola | k | at -> -at
   *
   *  Boolean in triple tells if word has been parsed with paranthesis. i.e. (jalkain)
   */
  def resolveCase(triple: (NomineMorphemes, String, Boolean), root: String, gradation: Gradation): Declension =
    import GradationType.*
    val endingWithGradation = triple._2.drop(root.length)
    val (ending, tpe) = if endingWithGradation.startsWith(gradation.strong) then endingWithGradation.drop(gradation.strong.length) -> NomineGradationType.Strong
      else endingWithGradation.drop(gradation.weak.length) -> NomineGradationType.Weak
    Declension(triple._1, ending.map(Ch(_)), tpe)

  import MorphemesUtils._

  def resolveNonGradation(exampleDeclensions: NomineExampleDeclensions): DeclensionRules =
    val casedWords = exampleDeclensions.cases.map(_._2)
    val rootSuggestion = LongestStartingSubstring(casedWords)
    val root = if(Letters.isConsonant(exampleDeclensions.lemma.last) && exampleDeclensions.lemma.length - rootSuggestion.length == 1)
      then rootSuggestion.dropRight(1) else rootSuggestion
    val drop = exampleDeclensions.lemma.length - root.length

    val nominative = exampleDeclensions.cases.find(_._1 == Nom::S).get
    val replacementVowels = nominative._2.drop(root.length).takeWhile(Letters.isVowel(_))
    val cases = exampleDeclensions.cases.map(a => resolveNonGradationCase(a._1, a._2, root, replacementVowels))
    DeclensionRules(exampleDeclensions.number, drop, false, replacementVowels, cases)

  def resolveNonGradationCase(morphemes: NomineMorphemes, word: String, root: String, replacementVowels: Seq[Char]): Declension =
    val ending = word.drop(root.length)
    val (start, end) = if morphemes != (Ill::S) then splitByFirstConsonant(ending) else (ending, "")
    val resultEnding = start.map(c => if(replacementVowels.contains(c)) then Rep(c) else Ch(c)) ++ end.map(Ch(_))
    Declension(morphemes, resultEnding, NomineGradationType.Nothing)


  def splitByFirstConsonant(str: String): (String, String) =
    val index = str.indexWhere(Letters.isConsonant(_))
    if(index == -1) then (str, "")
    else str.splitAt(index)

}

/***
 * Example: Seq("algorithm", "algotim", "algonat") -> "algo"
 */
object LongestStartingSubstring {
  def apply(list: Seq[String]): String =
    val sortedList = list.sorted
    val first = sortedList.head
    val last = sortedList.last
    var i = 0
    var result = ""
    while(i < first.length && i < last.length && first(i) == last(i))
      result += first(i)
      i += 1
    result
}