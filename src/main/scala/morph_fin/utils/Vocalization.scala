package morph_fin.utils

enum Vocalization:
  case FrontVowel // a, o, u
  case BackVowel //ä, ö, y, i, e

object VocalizationUtils {

  def resolveVocalization(lemma: String): Vocalization =
    if lemma.forall(char => char != 'a' && char != 'o' && char != 'u')
    then Vocalization.BackVowel
    else Vocalization.FrontVowel

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
