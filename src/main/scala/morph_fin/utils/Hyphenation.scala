package morph_fin.utils


enum LetterType:
  case Vowel, Consonant

object Hyphenation {

  val vowels = Seq('a', 'e', 'o', 'i', 'u', 'y', 'ä', 'ö')
  val diftongs1 = Seq("ai", "ei", "oi", "äi", "öi", "ey", "äy", "öy", "au", "eu", "ou", "ui", "yi", "iu", "iy")
  val diftongs2 = Seq("ie", "uo", "yö")

  private def iter(word: String, i: Int, collected: Option[Char] = None, firstSyllable: Boolean = true): Seq[String] =
    if word.isEmpty then Nil
    else if i+1 >= word.length then Seq(word)
    else
      val a1 = word.charAt(i)
      val a2 = word.charAt(i+1)
      val subs = word.substring(i, i + 2)
      val threeVowels = collected.map(isVowel(_)).getOrElse(false) && isVowel(a1) && isVowel(a2)
      val isDiftong = isDiftong1(subs) || (isDiftong2(subs) && firstSyllable)
      (isVowel(a1), isVowel(a2)) match {
        case (true, true) if (!isDiftong && a1 != a2) || threeVowels =>
          word.substring(0, i+1) +: iter(word.substring(i+1), 0, None, false)
        case (false, true) if i != 0 => word.substring(0, i) +: iter(word.substring(i), 0, None, false)
        //If word has gradation with empty weak gradation, then hyphenation also occurs in gradation location.
        case _ if a2 == '^'          => word.substring(0, i+1) +: iter(word.substring(i+2), 0, None, false)
        case _                       => iter(word, i+1, Some(a1), firstSyllable)
      }

  def split(word: String): Seq[String] =
    iter(word, 0)

  def getForEnding(ending: String): Seq[String] =
    iter(ending, 0, None, false)

  //Examples: laa^an -> laa'an, reiitin -> rei'itin
  def addApostrophes(word: String): String =
    assert(word.nonEmpty)
    val list = split(word)
    var result = list.head
    for(syllable <- list.tail) {
      if Letters.isVowel(result.last) && syllable.head == result.last then result +=  "\'" + syllable
      else result += syllable
    }
    result

  inline def isVowel(letter: Char) = vowels.contains(letter)
  inline def isDiftong1(str: String) = diftongs1.contains(str)
  inline def isDiftong2(str: String) = diftongs2.contains(str)
}


