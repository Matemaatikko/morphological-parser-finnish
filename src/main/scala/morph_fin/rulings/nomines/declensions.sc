import morph_fin.kotus_format.{Bending, Entry, EntryToWord, KotusWord}
import morph_fin.rulings.Print
import morph_fin.rulings.nomines.{GenerateDeclensionWords, LoadAndParseNomineRules, ResultWord, Word}
import morph_fin.rulings.verbs.LoadAndParseVerbRules

val nomineRulings = LoadAndParseNomineRules.rules

def getWord(word: String, number: Int, gradationLetter: Option[Char] = None) =
  EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get

def printA(words: Seq[ResultWord]) =
  println(words.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")

def declesions(word: Word) = GenerateDeclensionWords.apply(nomineRulings, word)

def printB(word: String, rule: Int, gradationLetterOpt: Option[Char] = None) =
  val results = declesions(getWord(word, rule, gradationLetterOpt))
  printA(results)


//printB("tie", 19)
printB("ies", 41, Some('D'))
printB("kaunis", 41)
