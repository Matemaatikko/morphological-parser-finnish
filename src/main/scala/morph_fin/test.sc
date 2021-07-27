import morph_fin._
import morph_fin.kotus_format._
import morph_fin.rulings._

import java.io.File
import scala.io.{Codec, Source}

val path = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish"
val rulingLocations = "/src/main/files/rules/"

val location = path + rulingLocations

val filename = location + "nomine_rules.txt"
val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines) yield line

val rulings = new NomineRulesParser(content.mkString("\n").iterator).parse

val genRulings = rulings.map(GenerateRuling(_))

def prin(word: String, number: Int, gradationLetter: Option[Char]) =
  val word1 = GenerateCases.getWord(Entry(KotusWord.Word(word), Some(Bending(number, gradationLetter))))
  val cases = GenerateCases(genRulings, word1)
  println(cases.mkString("\n"))
end prin

//alfasäteet, beetasäteet, diplomaattisuhteet, housunkannattimet, hämäläismurteet, idänsuhteet, ikenet, illanistujaiset, istujaiset, jakopuitteet, kaakkoismurteet, kaikusuhteet, kauppatieteet, keliolosuhteet, korkeussuhteet, koulutarvikkeet, kuteet, lenkkivaatteet, liinavaatteet, likavaatteet, lounaismurteet, LVI-työt, lyydiläismurteet, lännensuhteet, länsisuhteet, mittasuhteet, olosuhteet, ompelutarvikkeet, perhesiteet, perhesuhteet, perintöhopeat, petivaatteet, piirustustarvikkeet, pitkäthousut, pitovaatteet, pukimet, pyhävaatteet, pöytähopeat, rohtimet, ruokatarpeet, ruumisvaatteet, savakkomurteet, sisävaatteet, sosiaalitieteet, stereokuulokkeet, sänkyvaatteet, sääolosuhteet, toukotyöt, tykötarpeet, työolosuhteet, ulkomaansuhteet, vanginvaatteet, vatsanpeitteet, vetimet, äyrämöismurteet, ääripiirteet


prin("hapsi", 7, None)
prin("säde", 48, Some('F'))
/*genRulings(18)
genRulings(18).cases.mkString("\n")
prin("työ", 19, None)*/

prin("kettu", 1, Some('C'))
/*
prin("puu", 18)

val aavikko4 = Word("aavikko", 4, Some(Gradation("kk", "k", GradationType.Strong)))
val cases = GenerateCases(genRulings, aavikko4)
println(cases.mkString("\n"))

val aiti4 = Word("äiti", 5, Some(Gradation("t", "d", GradationType.Strong)))
val cases2 = GenerateCases(genRulings, aiti4)
println(cases2.mkString("\n"))

val aima10 = Word("äimä", 10, None)
val cases3 = GenerateCases(genRulings, aima10)
println(cases3.mkString("\n"))


val ehdoton34 = Word("ehdoton", 34, Some(Gradation("tt", "t", GradationType.Weak)))
val cases4 = GenerateCases(genRulings, ehdoton34)
println(cases4.mkString("\n"))
*/

def filterAway(word: String):Boolean =
  word.endsWith("tta")
    || word.endsWith("äin")
    || word.endsWith("päiten")
    || word.endsWith("tta")
    || word.endsWith("lle")
    || word.endsWith("lla")
    || word.endsWith("llä")
    || word.endsWith("lta")
    || word.endsWith("ltä")
    || word.endsWith("ssä")
    || word.endsWith("ssa")
    || word.endsWith("sta")
    || word.endsWith("stä")
    || word.endsWith("sin")
    || word.endsWith("oon")
    || word.endsWith("iin")
    || word.endsWith("ntä")
    || word.endsWith("ihin")
    || word.endsWith("ksi")
    || word.endsWith("ana")
    || word.endsWith("aa")
    || word.endsWith("een")
    || word.endsWith("ain")
    || word.endsWith("aan")
    || word.endsWith("ään")
    || word.endsWith("nsä")


val result: Seq[UpdatedWord] = ReformatKotus(genRulings)
val errors = result.flatMap(word => word match {
  case UpdatedWord.CompoundError(value, _) => Some(value)
  case _                                => None
})
print(errors.mkString("\n"))

ReformatKotus.save(result)
/*

print(errors.filter(_.last == 't').mkString("\n"))

print(errors.filter(_.last != 't').filterNot(filterAway(_)).mkString("\n"))*/
