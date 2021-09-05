import morph_fin.kotus_format.{Bending, Entry, EntryToWord, KotusWord}
import morph_fin.rulings.nouns.{DeclensionRule, DeclensionUtils, InflectedWord, LoadAndParseNomineRules, PossessiveSuffixGeneration, Word}
import morph_fin.rulings.verbs.LoadAndParseVerbRules

val rules = LoadAndParseNomineRules.rules
given Seq[DeclensionRule] = rules

def getWord(word: String, number: Int, gradationLetter: Option[Char] = None) =
  EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get

def printA(words: Seq[InflectedWord]) =
  println(words.map(a => a.word.toString + " : " + a.morphemes.toString).mkString("\n"))
  println("============================")

def declesions(word: Word) = DeclensionUtils.generateDeclensions(word)

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
printB("likempi", 16, Some('H'))
printB("varas", 41, Some('D'))
printB("aakkosto", 2, None)
printB("aallokas", 41, Some('A'))
printB("aie", 48, Some('D'))
printB("ajos", 39, None)
printB("liittoutuneet", 47, None)

printB("kantele", 49, None)
printB("kannel", 49, Some('J'))

