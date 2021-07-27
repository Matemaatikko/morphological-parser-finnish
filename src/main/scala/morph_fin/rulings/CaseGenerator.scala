package morph_fin.rulings

import morph_fin.rulings.CaseEnding.GradationEnding
import morph_fin._
import morph_fin.kotus_format.Entry

import java.nio.charset.StandardCharsets

val letterMap = Map(
  'A' -> 0, 'B' -> 1, 'C' -> 2, 'D' -> 3,
  'E' -> 4, 'F' -> 5, 'G' -> 6, 'H' -> 7,
  'I' -> 8, 'J' -> 9, 'K' -> 10, 'L' -> 11,
  'M' -> 12, 'N' -> 13
)


val gradationMap = Seq(
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


enum CaseEnding(val cse: Case, val tpe: Type, val ending: String):
  case Normal(
               override val cse: Case,
               override val tpe: Type,
               override val ending: String
             ) extends CaseEnding(cse, tpe, ending)
  case GradationEnding(
                        gradType: GradationType,
                        override val cse: Case,
                        override val tpe: Type,
                        override val ending: String
                      ) extends CaseEnding(cse, tpe, ending)

case class ExtensiveRuling(
                            number: Int,
                            drop: Int,
                            gradation: Option[Gradation],
                            cases: Seq[CaseEnding]
                          )


object GenerateRuling {

  def apply(ruling: Ruling): ExtensiveRuling =
    ruling.gradation match {
      case Some(gradation) => resolveGradation(ruling: Ruling, gradation)
      case None            => resolveNonGradation(ruling: Ruling)
    }

  import CaseEnding._

  def resolveGradation(ruling: Ruling, gradation: Gradation): ExtensiveRuling =
    val casedWords = ruling.cases.map(_._3)
    val root =  findRoot(casedWords)
    val updatedRoot = gradation.strong.length match{
      case 1 => root //One letter gradation
      case 2 => root.dropRight(1) //Two letter gradation
    }
    val drop = ruling.lemma.length - updatedRoot.length
    val cases = ruling.cases.map(resolveCase(_, updatedRoot, gradation))
    ExtensiveRuling(ruling.number, drop, Some(gradation), cases)

  def resolveCase(triple: (Case, Type, String), root: String, gradation: Gradation): GradationEnding =
    import GradationType._
    val ending = triple._3.drop(root.length)
    val (tpe, noGradationEnding) =
      if ending.startsWith(gradation.strong) then (Strong, ending.drop(gradation.strong.length))
      else (Weak, ending.drop(gradation.weak.length))
    GradationEnding(tpe, triple._1, triple._2, noGradationEnding)

  def resolveNonGradation(ruling: Ruling): ExtensiveRuling =
    val casedWords = ruling.cases.map(_._3)
    val root = findRoot(casedWords)
    val drop = ruling.lemma.length - root.length
    val cases = ruling.cases.map(a => Normal(a._1, a._2, a._3.drop(root.length)))
    ExtensiveRuling(ruling.number, drop, None, cases)


  def findRoot(list: Seq[String]): String =
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

case class Word(lemma: String, number: Int, gradation: Option[Gradation])
case class ResultWord(word: String, cse: Case, tpe: Type, lemma: String)

enum Vocalization:
  case FrontVowel // a, o, u
  case BackVowel //ä, ö, y, i, e




object GenerateCases {
  def getWord(entry: Entry): Word =
    Word(
      entry.word.value,
      entry.bending.get.rule,
      entry.bending.get.gradationLetter.map(
        l => {
          val (strong, weak) = gradationMap(letterMap(l))
          Gradation(strong, weak)
      })
    )

  val listOfSomeVowels = Seq('a', 'o', 'u', 'y', 'ä', 'ö')
  val listOfAllVowels = Seq('a', 'o', 'i', 'e', 'u', 'y', 'ä', 'ö')
  def apply(rulings: Seq[ExtensiveRuling], word: Word): Seq[ResultWord] =
    val ruling = rulings.find(_.number == word.number).get
    val root = word.lemma.dropRight(ruling.drop)
    val lastVowel = if listOfSomeVowels.contains(word.lemma.last) && word.number != 19 then Some(word.lemma.last) else None
    word.gradation match {
      case Some(gradation) if ruling.gradation.nonEmpty  => resolveRulingGradation(word.lemma, root, ruling, gradation, lastVowel)
      case Some(gradation) if ruling.gradation.isEmpty   => resolveWordGradation(word.lemma, root, ruling, gradation, lastVowel)
      case None if ruling.gradation.isEmpty              => resolveNoGradation(word.lemma, root, ruling, lastVowel)
      case None if ruling.gradation.nonEmpty             => resolveRulingGradation(word.lemma, root, ruling, ruling.gradation.get, lastVowel)
    }

  def resolveVocalization(lemma: String): Vocalization =
    if lemma.forall(char => char != 'a' && char != 'o' && char != 'u')
    then Vocalization.BackVowel
    else Vocalization.FrontVowel

  //Example: laatikko
  def resolveRulingGradation(lemma: String, root: String, ruling: ExtensiveRuling, gradation: Gradation, lastVowel: Option[Char]) =
    import CaseEnding._
    val vocalization = resolveVocalization(lemma)
    ruling.cases.map(_.asInstanceOf[GradationEnding]).map(
      ending => {
        val grad = if ending.gradType == GradationType.Weak then gradation.weak else gradation.strong
        val word = root + grad + updateEnding(ending, vocalization, lastVowel, true)
        ResultWord(word, ending.cse, ending.tpe, lemma)
      }
    )

  def updateEnding(ending: CaseEnding, vocalization: Vocalization, lastVowel: Option[Char], isGradation: Boolean): String =
    val result =
      if vocalization == Vocalization.BackVowel
      then ending.ending.map( _ match {
        case a if a == 'a' => 'ä'
        case a if a == 'o' => 'ö'
        case a if a == 'u' => 'y'
        case a             => a
      })
      else ending.ending
    val updateCondition =
      result.nonEmpty &&
        listOfSomeVowels.contains(result.head) &&
        lastVowel.nonEmpty && ( ending.tpe == Type.Singular || ending.cse == Case.Nominative) &&
        !isGradation

    val vowelUpdated = if updateCondition then
      lastVowel.get + result.drop(1)
    else result

    val illativeCondition =
      ending.cse == Case.Illative &&
        ending.tpe == Type.Singular &&
        lastVowel.nonEmpty
    if illativeCondition
    then vowelUpdated.map(c => if listOfSomeVowels.contains(c) then lastVowel.get else c)
    else vowelUpdated



  val allVowelsExcepte = Seq('a', 'o', 'u', 'i', 'ä', 'ö', 'y')


  //Examples: ien, parka
  def resolveWordGradation(lemma: String, root: String, ruling: ExtensiveRuling, gradation: Gradation, lastVowel: Option[Char]) =
    import CaseEnding._
    val vocalization = resolveVocalization(lemma)

    val (beforeGradation, afterGradation) = splitByGradationLocation(root, lemma, gradation)

    ruling.cases.map(_.asInstanceOf[Normal]).map(
      ending => {
        val word = beforeGradation +
          resolveGradationByType(lemma.last, ending.cse, ending.tpe, gradation) +
          afterGradation +
          updateEnding(ending, vocalization, lastVowel, true)
        ResultWord(word, ending.cse, ending.tpe, lemma)
      }
    )

  def splitByGradationLocation(root: String, lemma: String, gradation: Gradation): (String, String) =
    val gradationString = if allVowelsExcepte.contains(lemma.last) then gradation.strong else gradation.weak
    if listOfAllVowels.contains(root.last) then (root.dropRight(gradationString.length + 1) , root.last.toString)
    else if root.lastOption != gradationString.headOption then (root.dropRight(gradationString.length + 2) , root.takeRight(2))
    else (root.dropRight(gradationString.length) , "")



  def resolveGradationByType(lastLetterOfLemma: Char, cse: Case, tpe: Type, gradation: Gradation): String =
    import Case._
    import Type._
    if allVowelsExcepte.contains(lastLetterOfLemma) then
      (cse, tpe) match {
         case (Nominative, Singular)  => gradation.strong
         case (Partitive, Singular)   => gradation.strong
         case (Akkusative, Singular)  => gradation.strong
         case (Illative, Singular)    => gradation.strong
         case (Essive, Singular)      => gradation.strong
         case (_, Singular)           => gradation.weak

         case (Partitive, Plural)   => gradation.strong
         case (Genetive, Plural)    => gradation.strong
         case (Illative, Plural)    => gradation.strong
         case (Essive, Plural)      => gradation.strong
         case (Komitative, Plural)  => gradation.strong
         case (_, Plural)           => gradation.weak
      }
    else
      (cse, tpe) match {
        case (Nominative, Singular)  => gradation.weak
        case (Akkusative, Singular)  => gradation.weak
        case (Partitive, Singular)   => gradation.weak
        case (_, Singular)           => gradation.strong
        case (_, Plural)             => gradation.strong
      }

  def resolveNoGradation(lemma: String, root: String, ruling: ExtensiveRuling, lastVowel: Option[Char]) =
    import CaseEnding._
    val vocalization = resolveVocalization(lemma)
    ruling.cases.map(_.asInstanceOf[Normal]).map(
      ending => {
        val word = root + updateEnding(ending, vocalization, lastVowel, false)
        ResultWord(word, ending.cse, ending.tpe, lemma)
      }
    )
}