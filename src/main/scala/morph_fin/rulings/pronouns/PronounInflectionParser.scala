package morph_fin.rulings.pronouns

import morph_fin.rulings.morpheme.{Abessive, Ablative, Accusative, Adessive, Allative, Case, Comitative, Elative, Essive, Genitive, Illative, Inessive, Instructive, Morphemes, Nominative, Noun, Partitive, Plural, Singular, Translative}
import morph_fin.rulings.nouns.{InflectedWord, StructuredWord}
import morph_fin.rulings.{GradationType, rules}
import morph_fin.rulings.rules.{DeclensionRule, GenerateDeclensionRules, Gradation, NounExampleDeclensions, NounRulesParser}
import morph_fin.utils.{FilesLocation, Parser}

import scala.io.{Codec, Source}


object LoadPronounInflections {

  def apply(): Seq[PronounIflection] = {
    val filename = FilesLocation.rules_path  + "/pronoun_rules.txt"
    val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines) yield line
    new PronounInflectionParser(content.mkString("\n").iterator).parse
  }

}

case class PronounIflection(lemma: String, inflections: Seq[InflectedWord])

class PronounInflectionParser(stream: Iterator[Char])  extends Parser(stream) {

  def parse: Seq[PronounIflection] =
    skipWhiteSpaces
    skipComments
    doUntil(parseNextEntry, peek !=  '<')

  def skipComments: Unit =
    if(peek == '#') doUntil(consume, peek == '\n')
    skipWhiteSpaces
    if(peek == '#') skipComments

  inline def parseNextEntry: PronounIflection =
    skipComments
    skip('<')
    val pronoun = collectUntil( peek == '>')
    skip('>')
    val inflections = doUntil(parseLine, peek == '<' || peek == '#')
      .toSeq
      .flatMap(entry => entry._2.map(a => InflectedWord(StructuredWord(a, "", ""), entry._1, pronoun)))
    skipComments
    PronounIflection(pronoun, inflections)

  inline def parseLine: (Morphemes, Seq[String]) =
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

  inline def removeParanthesis(word: String): String =
    if word.contains("(")
    then word.replace("(", "").replace(")", "")
    else word

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
