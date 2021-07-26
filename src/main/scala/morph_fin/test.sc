import morph_fin.NomineRulesParser
import morph_fin.GenerateRuling

import java.io.File
import scala.io.Source

val path = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish"
val rulingLocations = "/src/main/files/rules/"

val location = path + rulingLocations

val filename = location + "nomine_rules.txt"
val content = for (line <- Source.fromFile(filename).getLines) yield line

val rulings = new NomineRulesParser(content.mkString("\n").iterator).parse
rulings(15)


GenerateRuling(rulings(15))