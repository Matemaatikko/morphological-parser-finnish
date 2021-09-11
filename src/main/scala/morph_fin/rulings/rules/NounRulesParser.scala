package morph_fin.rulings.rules

import morph_fin.rulings.*
import morph_fin.rulings.rules.{DeclensionRule, GenerateDeclensionRules}
import morph_fin.utils.{FilesLocation, Parser}

import scala.annotation.tailrec
import scala.io.{Codec, Source}


object LoadAndParseNomineRules {

  def apply(): Seq[NounExampleDeclensions] = {
    val filename = FilesLocation.rules_path  + "/noun_rules.txt"
    val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines) yield line
    new NounRulesParser(content.mkString("\n").iterator).parse
  }

  def rules: Seq[DeclensionRule] = {
    apply().map(GenerateDeclensionRules(_))
  }
}

case class Gradation(strong: String, weak: String)
case class NounExampleDeclensions(number: Int, lemma: String, gradation: Option[Gradation], cases: Seq[(Morphemes, String, Boolean)])

class NounRulesParser(stream: Iterator[Char]) extends Parser(stream){

  def parse: Seq[NounExampleDeclensions] =
    skipWhiteSpaces
    skipComments
    doUntil(parseNextEntry, peek !=  '<')

  def skipComments: Unit =
    if(peek == '#') doUntil(consume, peek == '\n')
    skipWhiteSpaces
    if(peek == '#') skipComments

  inline def parseNextEntry: NounExampleDeclensions =
    skipWhiteSpaces
    skip('<')
    val number = collectUntil( !peek.isDigit).toInt
    val gradation = parseGradation
    skip('>')
    val lines = doUntil(parseLine, peek == '<').toSeq
    val lemma = lines.find(tuple => isLemma(tuple._1)).get._2.head
    val wordList = lines.flatMap(a => a._2.map(b => (a._1, b._1, b._2)))
    NounExampleDeclensions(number, lemma._1, gradation, wordList)

  inline def isLemma(moprhemes: Morphemes): Boolean =
    moprhemes.is(Nominative, Singular)

  inline def parseGradation: Option[Gradation] =
    if peek == ':' then
      skip(':')
      skip('G')
      skip(':')
      val tpe = peek match {
        case 'S' => GradationType.Strong
        case 'W' => GradationType.Weak
      }
      consume
      skip(':')
      val strong = collectUntil( peek == '-')
      skip('-')
      val weak = collectUntil( peek == '>')
      Some(rules.Gradation(strong, weak))
    else None


  inline def parseLine: (Morphemes, Seq[(String, Boolean)]) =
    skipWhiteSpaces
    val grammaticalNumber = peek match {
      case 'S' => Singular
      case 'P' => Plural
    }
    consume
    skip(':')
    val caseString: String = consume.toString + consume.toString + consume.toString
    val cse = resolveCase(caseString)
    skip(':')
    skipWhiteSpaces
    val words = collectUntil(peek == '\n').split(' ')
    val paranthesesRemoved = words
      .map(removePsuffix(_))
      .map(removeParanthesis(_))
    skipWhiteSpaces
    (Noun ~ cse ~ grammaticalNumber, paranthesesRemoved)

  inline def removePsuffix(word: String): String =
    if word.trim.endsWith("-P") then word.trim.dropRight(2)
    else word.trim

  inline def removeParanthesis(word: String): (String, Boolean) =
    if word.contains("(")
    then word.replace("(", "").replace(")", "") -> true
    else word -> false

  inline def resolveCase(str: String): Case = str.toLowerCase match {
    case a: String if a == "nom" => Nominative
    case a: String if a == "gen" => Genitive
    case a: String if a == "acc" => Accusative
    case a: String if a == "par" => Partitive
    case a: String if a == "ine" => Inessive
    case a: String if a == "ela" => Elative
    case a: String if a == "ill" => Illative
    case a: String if a == "ade" => Adessive
    case a: String if a == "abl" => Ablative
    case a: String if a == "all" => Allative
    case a: String if a == "ess" => Essive
    case a: String if a == "tra" => Translative
    case a: String if a == "ins" => Instructive
    case a: String if a == "abe" => Abessive
    case a: String if a == "com" => Comitative
  }


}
