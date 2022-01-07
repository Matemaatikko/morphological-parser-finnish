import morph_fin.rulings.verbs.{AllConjugationUtils, ConjugationUtils}
import morph_fin.kotus_format.{KotusWord, *}
import morph_fin.rulings.morpheme._
import morph_fin.rulings.nouns.InflectedWord
import morph_fin.rulings.nouns.Word
import morph_fin.rulings.*
import morph_fin.rulings.rules.{ConjugationRule, DeclensionRule, LoadAndParseNounRules, LoadAndParseVerbRules}



given Seq[ConjugationRule] = LoadAndParseVerbRules.rules
given Seq[DeclensionRule] = LoadAndParseNounRules.rules

def getWord(word: String, number: Int, gradationLetter: Option[Char] = None) =
  EntryToWord(Entry(KotusWord.Word(word), Some(Inflection(number, gradationLetter)))).get

def printA(words: Seq[InflectedWord]) =
  println(words.map(a => a.word.toString + " : " + PrintMorphemes(a.morphemes)).mkString("\n"))
  println("============================")

def conjugations(word: Word) = ConjugationUtils.generateConjugations(word)
def allConjugations(word: Word) = AllConjugationUtils.generateAllConjugations(word)

def printB(word: String, rule: Int, gradationLetterOpt: Option[Char] = None) =
  val results = conjugations(getWord(word, rule, gradationLetterOpt))
  printA(results)

def printAll(word: String, rule: Int, gradationLetterOpt: Option[Char] = None) =
  val results = allConjugations(getWord(word, rule, gradationLetterOpt))
  //println(results.filter(_.morphemes.is(Finite ~ Indicative ~ Present ~ SingularFirst ~ Positive)).map(_.word.toString))
  printA(results)


println(LoadAndParseVerbRules.rules.find(_.ruleNumber == 52).get.cases.mkString("\n"))

printAll("kaikaa", 78, None)

