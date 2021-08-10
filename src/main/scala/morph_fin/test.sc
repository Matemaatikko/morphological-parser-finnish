import morph_fin.*
import morph_fin.inflection.GenerateInflectedWords
import morph_fin.kotus_format.*
import morph_fin.rulings.{Print, *}
import morph_fin.rulings.nomines.{GenerateDeclensionRules, GenerateDeclensionWords, Gradation, LoadAndParseNomineRules, NomineRulesParser, ResultWord, Word}
import morph_fin.rulings.verbs.{GenerateConjugatedWords, GenerateConjugationRules, LoadAndParseVerbRules, VerbRulesParser}
import morph_fin.utils.{FilesLocation, Hyphenation}

import java.io.File
import scala.collection.mutable
import scala.io.{Codec, Source}


val nomineRulings = LoadAndParseNomineRules.rules
val verbRulings = LoadAndParseVerbRules.rules

def getWord(word: String, number: Int, gradationLetter: Option[Char] = None) =
  EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get

def printA(words: Seq[ResultWord]) =
  println(words.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")

def declesions(word: Word) = GenerateDeclensionWords.apply(nomineRulings, word)

val results = declesions(getWord("valtio", 3))
printA(results)


