package morph_fin.rulings.rules

import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Accusative, Illative, Morphemes, Nominative, Singular}
import morph_fin.rulings.nouns.*
import morph_fin.utils.{Letters, LongestStartingSubstring}


/**
 * Definition replacement system
 * =============================
 * root = common prefix for all inflections
 * word = root + ending
 * --------------------
 * All vowels in ending of lemma are defined as replacement vowels.
 * Example: "taikoa"
 * root = tai, ending = koa, replacement vowels = o, a
 * Similarly corresponding replacement vowels for "taintua" are "u" and "a" (Both words have same inflection class).
 */

enum RepChar:
  case Ch(c: Char)
  case Rep(key: Char)

import morph_fin.rulings.rules.RepChar.*

object Replacement {

  /**
   * Example:
   * Word = kallio, ending = Seq(Ch(t), Rep(a))
   * Result: Map('o' -> 'a')
   */
  def resolveMap(word: String, ending: Seq[RepChar]): Map[Char, Char] =
    assert(word.length >= ending.length)
    (0 until ending.length).flatMap(i => ending(i) match {
      case RepChar.Ch(c) => None
      case RepChar.Rep(c) => Some(c -> word(word.length - ending.length + i))
    }).toMap
}

extension (str: String)
  def hasEnding(ending: Seq[RepChar]): Boolean = str.length >= ending.length &&
    (0 until ending.length).forall(i => ending(i) match {
      case RepChar.Ch(c) => c == str(str.length - ending.length + i)
      case RepChar.Rep(c) => Letters.isVowel(str(str.length - ending.length + i))
    })


extension(ending: Seq[RepChar])
  def create(map: Map[Char, Char]): String =
    ending.map(_ match {
      case RepChar.Ch(c) => c
      case RepChar.Rep(c) => map.get(c).getOrElse(c)
    }).mkString("")

extension(list: Seq[RepChar])
  def value: String = list.toList match {
    case Ch(c) :: tail => c + tail.value
    case Rep(c) :: tail => c + tail.value
    case Nil           => ""
  }


case class Declension(morphemes: Morphemes, ending: Seq[RepChar], gradationTypeOpt: Option[GradationType])
case class DeclensionRule(
                            ruleNumber: Int,
                            drop: Int,
                            isGradation: Boolean,
                            replacementVowels: Seq[Char],
                            cases: Seq[Declension]
                          )

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

  def apply(exampleDeclensions: NounExampleDeclensions): DeclensionRule =
    exampleDeclensions.gradation match {
      case Some(gradation) => resolveGradation(exampleDeclensions, gradation)
      case None            => resolveNonGradation(exampleDeclensions)
    }

  import Declension.*

  //===========================================

  def resolveGradation(exampleDeclensions: NounExampleDeclensions, gradation: Gradation): DeclensionRule =
    //Resolve root. Resolvation is based on nomine_rules.txt file analysation
    val drop = exampleDeclensions.number match {
      case 35 => 4
      case 27 | 31 => 2
      case _  => 3
    }

    val root = exampleDeclensions.lemma.dropRight(drop)

    val replacementDrop: Int = exampleDeclensions.number match {
      case 4 | 14 | 16 | 27 | 28 | 31 => 1
      case  34 | 35     => 2
    }
    val replacementVowels = resolveReplacementVowels(exampleDeclensions, exampleDeclensions.lemma.length - replacementDrop)

    val tsGradation = Seq(27, 28, 31).contains(exampleDeclensions.number)

    //Resolve cases
    val cases = exampleDeclensions.cases
      .map(resolveGradationCase(_, root, gradation, replacementVowels, tsGradation))
      .filterNot(_.morphemes.is(Accusative))

    DeclensionRule(exampleDeclensions.number, drop, true, replacementVowels, cases)


  /**
   *  Removes root and gradation from cased word to resolve ending.
   *  Example: solakat -> sola | k | at -> -at
   *
   *  Boolean in triple tells if word has been parsed with paranthesis. i.e. (jalkain)
   */
  def resolveGradationCase(
                            triple: (Morphemes, String, Boolean),
                            root: String,
                            gradation: Gradation,
                            replacementVowels: Seq[Char],
                            tsGradation: Boolean
                          ): Declension =
    import GradationType.*
    val endingWithGradation = triple._2.drop(root.length)
    val (ending, tpe) =
      if tsGradation && endingWithGradation.startsWith(gradation.strong.replace("t", "s")) then
        endingWithGradation.drop(gradation.strong.length) -> Strong
      else if endingWithGradation.startsWith(gradation.strong) then
        endingWithGradation.drop(gradation.strong.length) -> Strong
      else
        endingWithGradation.drop(gradation.weak.length) -> Weak


    val updatedEnding = updateEndingWithReplacementRules(ending, triple._1, replacementVowels)

    Declension(triple._1, updatedEnding, Some(tpe))


  //===========================================

  def resolveNonGradation(exampleDeclensions: NounExampleDeclensions): DeclensionRule =
    val casedWords = exampleDeclensions.cases.map(_._2)
    val rootSuggestion = LongestStartingSubstring(casedWords)
    val root = if(Letters.isConsonant(exampleDeclensions.lemma.last) && exampleDeclensions.lemma.length - rootSuggestion.length == 1)
      then rootSuggestion.dropRight(1) else rootSuggestion
    val drop = exampleDeclensions.lemma.length - root.length

    val replacementVowels = resolveReplacementVowels(exampleDeclensions, root.length)
    val cases = exampleDeclensions.cases
      .map(a => resolveNonGradationCase(a._1, a._2, root, replacementVowels))
      .filterNot(_.morphemes.is(Accusative))
    DeclensionRule(exampleDeclensions.number, drop, false, replacementVowels, cases)

  def resolveNonGradationCase(morphemes: Morphemes, word: String, root: String, replacementVowels: Seq[Char]): Declension =
    val ending = word.drop(root.length)
    val resultEnding = updateEndingWithReplacementRules(ending, morphemes, replacementVowels)
    Declension(morphemes, resultEnding, None)

  //==================================

  private inline def resolveReplacementVowels(exampleDeclensions: NounExampleDeclensions, drop: Int): String =
    val nominative = exampleDeclensions.cases.find(_._1.is(Nominative, Singular)).get
    nominative._2.drop(drop).takeWhile(Letters.isVowel(_))

  private inline def updateEndingWithReplacementRules(ending: String, morphemes: Morphemes, replacementVowels: Seq[Char]): Seq[RepChar] =
    val (start, end) = if !morphemes.is(Illative, Singular) || ending.endsWith("seen") then splitByFirstConsonant(ending) else (ending, "")
    start.map(c => if(replacementVowels.contains(c)) then Rep(c) else Ch(c)) ++ end.map(Ch(_))

  def splitByFirstConsonant(str: String): (String, String) =
    val index = str.indexWhere(Letters.isConsonant(_))
    if(index == -1) then (str, "")
    else str.splitAt(index)

}
