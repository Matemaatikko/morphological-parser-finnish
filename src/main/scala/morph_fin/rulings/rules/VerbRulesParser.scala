package morph_fin.rulings.rules

import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{AInfinitive, AInfinitiveLong, Abessive, Ablative, Accusative, Active, Adessive, AgentParticiple, Allative, Case, Comitative, Conditional, EInfinitive, Elative, Essive, Finite, Genitive, Illative, Imperative, Imperfect, Indicative, Inessive, Infinitive, InfinitiveIV, InfinitiveV, Instructive, MAInfinitive, Mode, Morphemes, Negative, NegativeParticiple, Nominative, Partitive, Passive, Perfect, Person, Pluperfect, Plural, PluralFirst, PluralSecond, PluralThird, Positive, Potential, Present, Singular, SingularFirst, SingularSecond, SingularThird, Tempus, Translative, nUtParticiple, tUParticiple, vAParticiple}
import morph_fin.rulings.rules.{ConjugationRule, GenerateConjugationRules, Gradation}
import morph_fin.utils.{FilesLocation, Parser}

import java.io.File
import scala.annotation.tailrec
import scala.io.{Codec, Source}


object LoadAndParseVerbRules {

  def apply(): Seq[VerbExampleConjugations] = {
    val filename = FilesLocation.rules_path  + "/verb_rules.txt"
    val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines) yield line
    new VerbRulesParser(content.mkString("\n").iterator).parse
  }

  def rules: Seq[ConjugationRule] = {
    apply().map(GenerateConjugationRules(_))
  }
}

case class VerbExampleConjugations(number: Int, lemma: String, gradation: Option[Gradation], cases: Seq[(Morphemes, String)])

class VerbRulesParser(stream: Iterator[Char]) extends Parser(stream){

  def parse: Seq[VerbExampleConjugations] =
    skipWhiteSpaces
    skipComments
    doUntil(parseNextEntry, peek == '<')

  def skipComments: Unit =
    if(peek == '#') doUntil(consume, peek == '\n')
    skipWhiteSpaces
    if(peek == '#') skipComments

  inline def parseNextEntry: VerbExampleConjugations =
    skipComments
    skip('<')
    val number = collectUntil( !peek.isDigit).toInt
    val gradation = parseGradation
    skip('>')
    val lines = doUntil(parseLine, peek == '<').toSeq
    val lemma = lines.find(tuple => isLemma(tuple._1)).get._2.head
    val wordList = lines.flatMap(a => a._2.map(b => (a._1, b)))
    VerbExampleConjugations(number, lemma, gradation, wordList)

  inline def isLemma(moprhemes: Morphemes): Boolean =
    moprhemes.root match {
      case AInfinitive => true
      case _                  => false
    }

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

  inline def parseLine: (Morphemes, Seq[String]) =
    skipComments
    val morphemes = peek match {
      case 'P' | 'A' => parseNormal
      case 'I'       => parseInfinitive
      case 'C'       => parseParticiple
    }
    skipWhiteSpaces
    val words = collectUntil( peek == '\n').split(' ')
    val paranthesesRemoved = words.map(_.replace("(", "").replace(")", ""))
    skipWhiteSpaces
    (morphemes, paranthesesRemoved)

  //=========================================

  inline def parseNormal: Morphemes =
    val voice = parseVoice
    val modus = parseModus
    val tempus = parseTempus
    val persona = voice match {
      case Active  => parsePersona
      case Passive => Passive
    }
    val mode = parseMode
    Finite ~ modus ~ tempus ~ persona ~ mode

  inline def parsePersona: Person =
    val persona = peek2 match {
      case ('S', '1') => skipTimes(2); SingularFirst
      case ('S', '2') => skipTimes(2); SingularSecond
      case ('S', '3') => skipTimes(2); SingularThird
      case ('P', '1') => skipTimes(2); PluralFirst
      case ('P', '2') => skipTimes(2); PluralSecond
      case ('P', '3') => skipTimes(2); PluralThird
    }
    skip(':')
    persona

  inline def parseForm = peek match {
    case 'P' => consume; Plural
    case 'S' => consume; Singular
  }

  inline def parseVoice = peek match {
    case 'P' => skipAll("Pas:"); Passive
    case 'A' => skipAll("Akt:"); Active
  }

  inline def parseModus =  peek match {
    case 'I' => consume; peek match {
      case 'n' => skipAll("nd:"); Indicative
      case 'm' => skipAll("mp:"); Imperative
    }
    case 'C' => skipAll("Con:"); Conditional
    case 'P' => skipAll("Pot:"); Potential
  }

  inline def parseTempus: Tempus =
    peek3 match {
      case ('P', 'r', 'e') => skipAll("Pre:"); Present
      case ('P', 'e', 'r') => skipAll("Per:"); Perfect
      case ('I', 'm', 'p') => skipAll("Imp:"); Imperfect
      case ('P', 'l', 'p') => skipAll("Plp:"); Pluperfect
    }

  inline def parseMode: Mode =
    peek match {
      case 'N' => skipAll("Neg:"); Negative
      case _  =>  Positive
    }

  //=========================================

  def parseInfinitive: Morphemes =
    skipAll("Inf")
    peek match {
      case '1' => parseInfinitive1
      case '2' => parseInfinitive2
      case '3' => parseInfinitive3
      case '4' => parseInfinitive4
      case '5' => parseInfinitive5
    }

  inline def parseInfinitive1: Infinitive =
    skipAll("1:")
    peek match {
      case 'L' => skipAll("L:"); AInfinitiveLong
      case _   => AInfinitive
    }

  inline def parseInfinitive2: Morphemes =
    skipAll("2:")
    val cse = parseCase
    val voice = peek match {
      case 'A' => skipAll("Akt:");  Active
      case 'P' => skipAll("Pas:"); Passive
    }
    EInfinitive ~ cse ~ voice

  inline def parseInfinitive3: Morphemes =
    skipAll("3:")
    val cse = parseCase
    val voice = parseVoice
    MAInfinitive ~ cse ~ voice

  inline def parseInfinitive4: Morphemes =
    skipAll("4:")
    InfinitiveIV

  inline def parseInfinitive5: Morphemes =
    skipAll("5:")
    InfinitiveV

  inline def parseCase: Case =
    val caseString: String = consume.toString + consume.toString + consume.toString
    skip(':')
    resolveCase(caseString)

  inline def resolveCase(str: String): Case = str.toLowerCase match {
    case a: String if a == "nom" => Nominative
    case a: String if a == "gen" => Genitive
    case a: String if a == "akk" => Accusative
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
    case a: String if a == "kom" => Comitative
  }

  //=========================================

  inline def parseParticiple: Morphemes =
    skipAll("Par")
    peek match {
      case '1' => skipAll("1:"); vAParticiple ~ Active
      case '2' => skipAll("2:"); vAParticiple ~ Passive
      case '3' => skipAll("3:"); nUtParticiple
      case '4' => skipAll("4:"); tUParticiple
      case '5' => skipAll("5:"); AgentParticiple
      case '6' => skipAll("6:"); NegativeParticiple
    }

}
