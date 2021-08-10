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

def nprint(word: String, number: Int, gradationLetter: Option[Char]) =
  val word1 = EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get
  val cases = GenerateDeclensionWords(nomineRulings, word1)
  println(cases.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end nprint

def vprint(word: String, number: Int, gradationLetter: Option[Char]) =
  val word1 = EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get
  val cases = GenerateConjugatedWords(verbRulings, word1)
  println(cases.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end vprint

def printNomine(word: Word) =
  val cases = GenerateDeclensionWords(nomineRulings, word)
  println(cases.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end printNomine

def printVerb(word: Word) =
  val cases = GenerateConjugatedWords(verbRulings, word)
  println(cases.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end printVerb

def printA(words: Seq[ResultWord]) =
  println(words.map(a => a.word.toString + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")

def timed[A](fun: => A): A = {
  val start = System.currentTimeMillis()
  val result = fun
  val end = System.currentTimeMillis()
  println(s"Timed: ${end - start} ms")
  result
}

//val kotusUpdated = LoadUpdatedKotus.apply()


//vprint("abstraktistua", 52, None)

//timed(ReformatKotus.reformat)

//timed(LoadUpdatedKotus.apply())

//import morph_fin.inflection.TargetFile
//timed(GenerateInflectedWords.apply(TargetFile.Compound))

//
//val lines: Seq[Entry] = (for(line: String <- Source.fromFile(fileName)(Codec.UTF8).getLines)
//  yield
//    if(line.startsWith("<st>")) Some(ParseLine(line))
//    else None
//).flatten.toSeq
//
//val nomines = lines.flatMap(EntryToWord(_)).filter(_.ruleNumber < 50 )
//val verbs = lines.flatMap(EntryToWord(_)).filter(a => a.ruleNumber > 51 && a.ruleNumber < 99)
//var usedCases = mutable.Buffer[(Int, Option[Gradation])]()

/*
for(a <- objects) {
  if !usedCases.contains(a.ruleNumber -> a.gradation) then
    usedCases += a.ruleNumber -> a.gradation
    println(s"${a.ruleNumber -> a.gradation -> a.lemma}")
    println("-------------------")
    printNomine(a)
}
*/

//
//for(a <- verbs) {
//  if !usedCases.contains(a.ruleNumber -> a.gradation) then
//    usedCases += a.ruleNumber -> a.gradation
//    println(s"${a.ruleNumber -> a.gradation -> a.lemma}")
//    println("-------------------")
//    printVerb(a)
//}

//verbRulings.find(_.ruleNumber == 64).get.cases.mkString("\n")
//
//val IndPreS3 = VerbMophemes.Standard(Modus.Indicative, Tempus.Presens, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Positive)
//val results = verbs
//  .filter(_.ruleNumber == 64)
//  .map(word => GenerateVerbBendings(verbRulings, word))
//  .flatMap(words => words.find(_.morphemes == IndPreS3))
//  .mkString("\n")
//print(results)

/*val results = objects
  .filterNot(word => GenerateNomineBendings.getBending(nomineRulings, word).isGradation)
  .filter(word => GradationHandler.resolveGradationType(word.lemma.last, NomineMorphemes(Case.Nominative, Form.Singular)) == GradationType.Weak)
  .filter(_.gradation.get.weak.isEmpty)
  .map(word => (word, GenerateNomineBendings.getRoot(nomineRulings, word)))
  .map(tuple => (tuple._1, tuple._2, GradationHandler.splitByGradationLocation(tuple._2, tuple._1.gradation.get)))
  .map(tuple => (tuple._1.lemma, tuple._2, tuple._3, tuple._1.gradation))
  .mkString("\n")
print(results)*/


val results = GenerateDeclensionWords.generateWithPossessiveSuffixes(nomineRulings, getWord("askele", 49))
printA(results)


