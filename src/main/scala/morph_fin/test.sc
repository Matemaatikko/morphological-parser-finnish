import morph_fin.*
import morph_fin.kotus_format.*
import morph_fin.rulings.{Print, *}
import morph_fin.rulings.nomines.{GenerateNomineBendings, GenerateNomineRules, Gradation, NomineRulesParser, Word}
import morph_fin.rulings.verbs.{GenerateVerbBendings, GenerateVerbRules, VerbRulesParser}
import morph_fin.utils.Hyphenation

import java.io.File
import scala.collection.mutable
import scala.io.{Codec, Source}

val path = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish"
val rulingLocations = "/src/main/files/rules/"

val location = path + rulingLocations

val filename = location + "nomine_rules.txt"
val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines) yield line

val filename2 = location + "verb_rules.txt"
val content2 = for (line <- Source.fromFile(filename2)(Codec.UTF8).getLines) yield line

val nomineRulings1 = new NomineRulesParser(content.mkString("\n").iterator).parse
val verbRulings1 = new VerbRulesParser(content2.mkString("\n").iterator).parse

val nomineRulings = nomineRulings1.map(GenerateNomineRules(_))
val verbRulings = verbRulings1.map(GenerateVerbRules(_))

def prin(word: String, number: Int, gradationLetter: Option[Char]) =
  val word1 = EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get
  val cases = GenerateNomineBendings(nomineRulings, word1)
  println(cases.map(a => a.word + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end prin

def vin(word: String, number: Int, gradationLetter: Option[Char]) =
  val word1 = EntryToWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter)))).get
  val cases = GenerateVerbBendings(verbRulings, word1)
  println(cases.map(a => a.word + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end vin

def printNomine(word: Word) =
  val cases = GenerateNomineBendings(nomineRulings, word)
  println(cases.map(a => a.word + " : " + Print(a.morphemes)).mkString("\n"))
  println("============================")
end printNomine


/*val lines: Seq[Entry] = (for(line: String <- Source.fromFile(fileName)(Codec.UTF8).getLines)
  yield
    if(line.startsWith("<st>")) Some(ParseLine(line))
    else None
).flatten.toSeq

val objects = lines.flatMap(EntryToWord(_)).filter(_.gradation.nonEmpty).filter(_.ruleNumber < 50)
var usedCases = mutable.Buffer[(Int, Option[Gradation])]()

for(a <- objects) {
  if !usedCases.contains(a.ruleNumber -> a.gradation) then
    usedCases += a.ruleNumber -> a.gradation
    println(s"${a.ruleNumber -> a.gradation -> a.lemma}")
    println("-------------------")
    printNomine(a)
}*/

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
vin("hypähtää", 53, Some('F'))

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

/*Hyphenation.apply("kettu").mkString("-")
Hyphenation.apply("öljytuikku").mkString("-")
Hyphenation.apply("öljysorastaa").mkString("-")
Hyphenation.apply("öljynviejämaa").mkString("-")
Hyphenation.apply("öljynjalostusteollisuus").mkString("-")
Hyphenation.apply("nähtävissä").mkString("-")
Hyphenation.apply("kihlakunnansyyttäjä").mkString("-")
Hyphenation.apply("syysilta").mkString("-")*/

//println(KotusHyphenation.apply().mkString("\n"))

/*
val result: Seq[UpdatedWord] = ReformatKotus(genRulings)

val errors = result.flatMap(word => word match {
  case a: UpdatedWord.Error => Some(a)
  case _                    => None
})
println("Errors: " + errors.length)
print(errors.filter(_.bendingOpt.nonEmpty).mkString("\n"))


print(errors.filter(_.bendingOpt.isEmpty).mkString("\n"))

ReformatKotus.save(result)
*/

