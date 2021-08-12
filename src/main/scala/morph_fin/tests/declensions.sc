import morph_fin.kotus_format.{Bending, Entry, EntryToWord, KotusWord}
import morph_fin.rulings.Print
import morph_fin.rulings.nomines.{DeclensionUtils, LoadAndParseNomineRules, ResultWord, Word}
import morph_fin.rulings.verbs.LoadAndParseVerbRules

val nomineRulings = LoadAndParseNomineRules.rules

def getWord(word: String, number: Int, gradationLetter: Option[Char] = None) =
  EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get

def printA(words: Seq[ResultWord]) =
  println(words.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")

def declesions(word: Word) = DeclensionUtils.addDeclesions(nomineRulings, word)

def printB(word: String, rule: Int, gradationLetterOpt: Option[Char] = None) =
  val results = declesions(getWord(word, rule, gradationLetterOpt))
  printA(results)


//printB("tie", 19)
printB("ies", 41, Some('D'))
printB("kaunis", 41)
printB("hapan", 33, Some('B')) //Still wrong (e-a in middle of ending)
printB("jälsi", 28, Some('I')) //Still wrong (ls gradation replacement)
printB("järkky", 1, Some('A'))
printB("kalterit", 6, None)
printB("kanki", 7, Some('G'))
printB("manne", 8, None)
