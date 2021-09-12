import morph_fin.kotus_format.{Entry, EntryToWord, Inflection, KotusWord}
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Genitive, PrintMorphemes, Singular}
import morph_fin.rulings.nouns.*
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNomineRules, LoadAndParseVerbRules}

val rules = LoadAndParseNomineRules.rules
given Seq[DeclensionRule] = rules

rules.find(_.ruleNumber == 34)

def getWord(word: String, number: Int, gradationLetter: Option[Char] = None) =
  EntryToWord(Entry(KotusWord.Word(word), Some(Inflection(number, gradationLetter)))).get

def printA(words: Seq[InflectedWord]) =
  println(words.map(a => a.word.toString + " : " + PrintMorphemes(a.morphemes)).mkString("\n"))
  println("============================")

def declesions(word: Word) = DeclensionUtils.generateDeclensions(word)
//def declesions(word: Word) = AllDeclensionUtils.generateAllDeclections(word)

def declesions2(word: String, rule: Int, gradationLetterOpt: Option[Char] = None) =
  declesions(getWord(word, rule, gradationLetterOpt))


def printB(word: String, rule: Int, gradationLetterOpt: Option[Char] = None) =
  val results = declesions(getWord(word, rule, gradationLetterOpt))
  printA(results)


//printB("tie", 19)
printB("ies", 41, Some('D'))
printB("kaunis", 41)
printB("hapan", 34, Some('B')) //Still wrong (e-a in middle of ending)
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


val gen = declesions2("kaunis", 41, None).find(_.morphemes.is(Singular, Genitive)).get
printA(ComparationUtils.generateSuperlativeInflections(gen))

val ge2 = declesions2("keltainen", 38, None).find(_.morphemes.is(Singular, Genitive)).get
printA(ComparationUtils.generateSuperlativeInflections(ge2))

val ge3 = declesions2("halpa", 9, Some('E')).find(_.morphemes.is(Singular, Genitive)).get
printA(ComparationUtils.generateComparativeInflections(ge3))

