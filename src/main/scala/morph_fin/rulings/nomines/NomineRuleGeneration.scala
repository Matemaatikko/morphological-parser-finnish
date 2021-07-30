package morph_fin.rulings.nomines

import morph_fin.rulings.*

case class NomineEnding(val morphemes: NomineMorphemes, val ending: String)

case class NomineBending(number: Int, drop: Int, isGradation: Boolean, cases: Seq[NomineEnding])

extension (bending: NomineBending)
  def findCase(morphemes: NomineMorphemes): NomineEnding =
    bending.cases.find(ending => ending.morphemes == morphemes).getOrElse(throw new Error("Non-comprehensive matching in rules"))


/**
 * Idea of the following method is to find endings for each morphemes based on example bendings.
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
object GenerateNomineRules {

  def apply(ruling: NomineExampleBending): NomineBending =
    ruling.gradation match {
      case Some(gradation) => resolveGradation(ruling: NomineExampleBending, gradation)
      case None            => resolveNonGradation(ruling: NomineExampleBending)
    }

  import NomineEnding.*

  def resolveGradation(ruling: NomineExampleBending, gradation: Gradation): NomineBending =
    //Resolve root. Resolvation is based on nomine_rules.txt file analysation
    val drop = if ruling.number == 35 then 4 else 3
    val root = ruling.lemma.dropRight(drop)

    //Resolve cases
    val cases = ruling.cases.map(resolveCase(_, root, gradation))
    NomineBending(ruling.number, drop, true, cases)

  /**
   *  Removes root and gradation from cased word to resolve ending.
   *  Example: solakat -> sola | k | at -> -at
   */
  def resolveCase(tuple: (NomineMorphemes, String), root: String, gradation: Gradation): NomineEnding =
    import GradationType.*
    val endingWithGradation = tuple._2.drop(root.length)
    val ending = if endingWithGradation.startsWith(gradation.strong) then endingWithGradation.drop(gradation.strong.length)
      else endingWithGradation.drop(gradation.weak.length)
    NomineEnding(tuple._1, ending)


  def resolveNonGradation(ruling: NomineExampleBending): NomineBending =
    val casedWords = ruling.cases.map(_._2)
    val root = ShortestSubstring(casedWords)
    val drop = ruling.lemma.length - root.length
    val cases = ruling.cases.map(a => NomineEnding(a._1, a._2.drop(root.length)))
    NomineBending(ruling.number, drop, false, cases)
}

/***
 * Example: Seq("algorithm", "algotim", "algonat") -> "algo"
 */
object ShortestSubstring {
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