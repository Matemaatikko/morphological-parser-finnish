import morph_fin.*
import morph_fin.kotus_format.*
import morph_fin.rulings.{Print, *}
import morph_fin.rulings.nomines.{GenerateNomineBendings, GenerateNomineRules, Gradation, LoadAndParseNomineRules, NomineRulesParser, Word}
import morph_fin.rulings.verbs.{GenerateVerbBendings, GenerateVerbRules, LoadAndParseVerbRules, VerbRulesParser}
import morph_fin.utils.{FilesLocation, Hyphenation}

import java.io.File
import scala.collection.mutable
import scala.io.{Codec, Source}

val nomineRulings = LoadAndParseNomineRules.rules
val verbRulings = LoadAndParseVerbRules.rules

def nprint(word: String, number: Int, gradationLetter: Option[Char]) =
  val word1 = EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get
  val cases = GenerateNomineBendings(nomineRulings, word1)
  println(cases.map(a => a.word + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end nprint

def vprint(word: String, number: Int, gradationLetter: Option[Char]) =
  val word1 = EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get
  val cases = GenerateVerbBendings(verbRulings, word1)
  println(cases.map(a => a.word + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end vprint

def printNomine(word: Word) =
  val cases = GenerateNomineBendings(nomineRulings, word)
  println(cases.map(a => a.word + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end printNomine


def printVerb(word: Word) =
  val cases = GenerateVerbBendings(verbRulings, word)
  println(cases.map(a => a.word + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end printVerb

def timed[A](fun: => A): A = {
  val start = System.currentTimeMillis()
  val result = fun
  val end = System.currentTimeMillis()
  println(s"Timed: ${end - start} ms")
  result
}

timed(ReformatKotus.generateBendings)

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


//vin("keriytyä", 52, Some('F'))
//vin("juosta", 70, None)

//vin("vastata", 73, None)
//vin("hypätä", 73, Some('B'))
//vprint("hypähtää", 53, Some('F'))
//vprint("aateloida", 62, None)
//vprint("juoda", 64, None)
//vprint("ahdata", 73, Some('F'))
//vprint("kadota", 74, Some('F'))
//vprint("ahertaa", 54, Some('K'))
//vprint("aientaa", 54, Some('J'))
//vprint("viedä", 64, None)

//prin("ies", 41, Some('D'))
//prin("ahdas", 41, Some('F'))
//prin("aaprotti", 5, Some('C'))
//
//prin("hapan", 33, Some('B'))
//prin("kiittimet", 33, Some('C'))
//prin("morsian", 33, None)

//prin("nukke", 8, Some('A'))
//prin("jeppe", 8, Some('B'))

//prin("hapsi", 7, None)
//prin("säde", 48, Some('F'))
//prin("työ", 19, None)
//
//prin("kettu", 1, Some('C'))
//prin("ien", 32, Some('D'))
//prin("aie", 48, Some('D'))
//prin("aika", 9, Some('D'))
//prin("taika", 9, Some('D'))
//prin("tuote", 48, Some('C'))

//prin("raffinoitunut", 47, None)
//prin("rahkeet", 48, Some('L'))
//prin("rattaat", 41, Some('C'))
//prin("reivit", 5, None)
//prin("riekkujaiset", 38, None)
//
//prin("valta", 9, Some('I'))
//prin("pitkä", 10, None)
//prin("iso", 1, None)
//prin("aivot", 1, None)
//
//prin("särkynyt", 47, None)
//
//prin("kahdeksa", 10, None)
//prin("vemmel", 49, Some('H'))


