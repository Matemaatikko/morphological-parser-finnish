package morph_fin.utils

import scala.annotation.tailrec

enum Vocalization:
  case FrontVowel // a, o, u
  case BackVowel //ä, ö, y, i, e

object VocalizationUtils {

  val frontVowels = Seq('a', 'o', 'u')
  val backVowels = Seq('ä', 'ö', 'y')

  def resolveVocalization(lemma: String): Vocalization =
    @tailrec
    def iter(value: String): Vocalization =
      if value.isEmpty then Vocalization.BackVowel
      else if frontVowels.contains(value.last) then Vocalization.FrontVowel
      else if backVowels.contains(value.last) then Vocalization.BackVowel
      else iter(value.dropRight(1))
    iter(lemma)

  def updateVocalization(ending: String, vocalization: Vocalization): String =
    if vocalization == Vocalization.BackVowel
    then ending.map( _ match {
      case a if a == 'a' => 'ä'
      case a if a == 'o' => 'ö'
      case a if a == 'u' => 'y'
      case a             => a
    })
    else ending.map( _ match {
      case a if a == 'ä' => 'a'
      case a if a == 'ö' => 'o'
      case a if a == 'y' => 'u'
      case a             => a
    })
}
