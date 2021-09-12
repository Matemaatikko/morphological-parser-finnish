import morph_fin.rulings.verbs.ConjugationUtils
import morph_fin.kotus_format.{KotusWord, *}
import morph_fin.rulings.nouns.InflectedWord
import morph_fin.rulings.nouns.Word
import morph_fin.rulings.Print
import morph_fin.rulings.rules.LoadAndParseVerbRules

val rules = LoadAndParseVerbRules.rules

def getWord(word: String, number: Int, gradationLetter: Option[Char] = None) =
  EntryToWord(Entry(KotusWord.Word(word), Some(Inflection(number, gradationLetter)))).get

def printA(words: Seq[InflectedWord]) =
  println(words.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")

def conjugations(word: Word) = ConjugationUtils.generateConjugations(rules, word)

def printB(word: String, rule: Int, gradationLetterOpt: Option[Char] = None) =
  val results = conjugations(getWord(word, rule, gradationLetterOpt))
  printA(results)


printB("viedä", 64, None)
