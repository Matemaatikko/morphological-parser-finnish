package morph_fin.utils

object Letters {

  val vowels = Seq('a', 'e', 'i', 'o', 'u', 'y', 'ä', 'ö', 'å')
  val consonants = Seq('b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z')

  inline def isVowel(letter: Char): Boolean = vowels.contains(letter)
  inline def isConsonant(letter: Char): Boolean = consonants.contains(letter)

}
