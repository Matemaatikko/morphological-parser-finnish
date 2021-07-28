import morph_fin.*
import morph_fin.kotus_format.*
import morph_fin.rulings.*
import morph_fin.utils.{Hyphenation, KotusHyphenation}

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


/*
prin("hapsi", 7, None)
prin("säde", 48, Some('F'))
genRulings(18)
genRulings(18).cases.mkString("\n")
prin("työ", 19, None)

prin("kettu", 1, Some('C'))
prin("ien", 32, Some('D'))
prin("aie", 48, Some('D'))

prin("raffinoitunut", 47, None)
prin("rahkeet", 48, Some('L'))
prin("rattaat", 41, Some('C'))
prin("ratas", 41, Some('C'))
prin("reivit", 5, None)
prin("riekkujaiset", 38, None)*/

//prin("valta", 9, Some('I'))
prin("pitkä", 10, None)
prin("iso", 1, None)
prin("aivot", 1, None)


/*Hyphenation.apply("kettu").mkString("-")
Hyphenation.apply("öljytuikku").mkString("-")
Hyphenation.apply("öljysorastaa").mkString("-")
Hyphenation.apply("öljynviejämaa").mkString("-")
Hyphenation.apply("öljynjalostusteollisuus").mkString("-")
Hyphenation.apply("nähtävissä").mkString("-")
Hyphenation.apply("kihlakunnansyyttäjä").mkString("-")
Hyphenation.apply("syysilta").mkString("-")*/

//println(KotusHyphenation.apply().mkString("\n"))

val result: Seq[UpdatedWord] = ReformatKotus(genRulings)

val errors = result.flatMap(word => word match {
  case a: UpdatedWord.Error => Some(a)
  case _                    => None
})
println("Errors: " + errors.length)
print(errors.mkString("\n"))

ReformatKotus.save(result)

