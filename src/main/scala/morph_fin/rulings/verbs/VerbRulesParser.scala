package morph_fin.rulings.verbs

import morph_fin.rulings.*
import morph_fin.rulings.Case.*
import morph_fin.rulings.nomines.Gradation
import morph_fin.utils.FilesLocation

import java.io.File
import scala.annotation.tailrec
import scala.io.{Codec, Source}


object LoadAndParseVerbRules {

  def apply(): Seq[VerbExampleBendind] = {
    val filename = FilesLocation.rules_path  + "/verb_rules.txt"
    val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines) yield line
    new VerbRulesParser(content.mkString("\n").iterator).parse
  }

  def rules: Seq[VerbBending] = {
    apply().map(GenerateVerbRules(_))
  }
}

case class VerbExampleBendind(number: Int, lemma: String, gradation: Option[Gradation], cases: Seq[(VerbMophemes, String)])

class VerbRulesParser(stream: Iterator[Char]) {

  var currentCharacter: Option[Char] = Some(' ')

  inline def peek: Char =
    currentCharacter.getOrElse(throw new Exception("Failed to retreive a character from empty stream!"))

  inline def consume: Char =
    val last = peek
    if (stream.hasNext) currentCharacter = Some(stream.next)
    else currentCharacter = None
    last

  inline def skip(value: Char, error: String = ""): Char =
    if peek != value then throw new Exception(error + peek)
    consume

  inline def skipAll(value: String, error: String = ""): Unit =
      value.foreach(c => skip(c))

  inline def skipWhiteSpaces =
    while (currentCharacter.exists(_.isWhitespace)) consume

  inline def skipWhiteSpacesUnlessLineBreak =
    while (currentCharacter.exists(a => a.isWhitespace && a != '\n'))  consume

  def collectUntil(condition: => Boolean): String =
    @tailrec
    def iter(result: String): String =
      if(!condition) iter(result + consume)
      else result
    iter("")

  def doUntil[A](fun: => A, condition: => Boolean): Seq[A] =
    @tailrec
    def iter(result: Seq[A]): Seq[A] =
      if(!condition) iter(result :+ fun)
      else result
    iter(Nil)

  def parse: Seq[VerbExampleBendind] =
    skipWhiteSpaces
    skipComments
    doUntil(parseNextEntry, !currentCharacter.contains('<'))

  def skipComments: Unit =
    if(currentCharacter == Some('#')) doUntil(consume, peek == '\n')
    skipWhiteSpaces
    if(currentCharacter == Some('#')) skipComments

  inline def parseNextEntry: VerbExampleBendind =
    skipComments
    skip('<')
    val number = collectUntil( !peek.isDigit).toInt
    val gradation = parseGradation
    skip('>')
    val lines = doUntil(parseLine, currentCharacter.isEmpty || currentCharacter.contains('<')).toSeq
    val lemma = lines.find(tuple => isLemma(tuple._1)).get._2.head
    val wordList = lines.flatMap(a => a._2.map(b => (a._1, b)))
    VerbExampleBendind(number, lemma, gradation, wordList)

  inline def isLemma(moprhemes: VerbMophemes): Boolean =
    moprhemes match {
      case VerbMophemes.InfinitiveI(Type.Short) => true
      case _                                    => false
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
      Some(nomines.Gradation(strong, weak))
    else None

  inline def parseLine: (VerbMophemes, Seq[String]) =
    skipComments
    val morphemes = peek match {
      case 'P' | 'A' => parseNormal
      case 'I'       => parseInfinitive
      case 'C'       => parseParticiple
    }
    skipWhiteSpaces
    val words = collectUntil(currentCharacter.isEmpty || currentCharacter.contains('\n')).split(' ')
    val paranthesesRemoved = words.map(_.replace("(", "").replace(")", ""))
    skipWhiteSpaces
    (morphemes, paranthesesRemoved)

  //=========================================

  inline def parseNormal: VerbMophemes.Standard =
    val voice = parseVoice
    val modus = parseModus
    val tempus = parseTempus
    val persona = voice match {
      case Voice.Active  => parsePersona
      case Voice.Passive => Persona.Passive
    }
    val mode = parseMode
    VerbMophemes.Standard(modus, tempus, persona, mode)

  inline def parsePersona: Persona.Active =
    val form = parseForm
    val persona = peek match {
      case '1' => consume; PersonaNumber.First
      case '2' => consume; PersonaNumber.Second
      case '3' => consume; PersonaNumber.Third
    }
    skip(':')
    Persona.Active(form, persona)

  inline def parseForm = peek match {
    case 'P' => consume; Form.Plural
    case 'S' => consume; Form.Singular
  }

  inline def parseVoice = peek match {
    case 'P' => skipAll("Pas:"); Voice.Passive
    case 'A' => skipAll("Akt:"); Voice.Active
  }

  inline def parseModus =  peek match {
    case 'I' => consume; peek match {
      case 'n' => skipAll("nd:"); Modus.Indicative
      case 'm' => skipAll("mp:"); Modus.Imperative
    }
    case 'C' => skipAll("Con:"); Modus.Conditional
    case 'P' => skipAll("Pot:"); Modus.Potential
  }

  inline def parseTempus: Tempus =
    peek match {
      case 'P' => consume; peek match {
        case 'r' => skipAll("re:"); Tempus.Presens
        case 'e' => skipAll("er:"); Tempus.Perfect
        case 'l' => skipAll("lp:"); Tempus.Pluperfect
      }
      case 'I' => skipAll("Imp:"); Tempus.Imperfect
    }

  inline def parseMode: Mode =
    peek match {
      case 'N' => skipAll("Neg:"); Mode.Negative
      case _  =>  Mode.Positive
    }

  //=========================================

  def parseInfinitive: VerbMophemes =
    skipAll("Inf")
    peek match {
      case '1' => parseInfinitive1
      case '2' => parseInfinitive2
      case '3' => parseInfinitive3
      case '4' => parseInfinitive4
      case '5' => parseInfinitive5
    }

  inline def parseInfinitive1: VerbMophemes.InfinitiveI =
    skipAll("1:")
    val tpe = peek match {
      case 'L' => skipAll("L:"); Type.Long
      case _ => Type.Short
    }
    VerbMophemes.InfinitiveI(tpe)

  inline def parseInfinitive2: VerbMophemes.InfinitiveII =
    skipAll("2:")
    val cse = parseCase
    val voice = peek match {
      case 'A' => skipAll("Akt:");  Voice.Active
      case 'P' => skipAll("Pas:"); Voice.Passive
    }
    VerbMophemes.InfinitiveII(cse, voice)

  inline def parseInfinitive3: VerbMophemes.InfinitiveIII =
    skipAll("3:")
    val cse = parseCase
    val voice = parseVoice
    VerbMophemes.InfinitiveIII(cse, voice)

  inline def parseInfinitive4: VerbMophemes.InfinitiveIV =
    skipAll("4:")
    val cse = parseCase
    VerbMophemes.InfinitiveIV(cse)

  inline def parseInfinitive5: VerbMophemes =
    skipAll("5:")
    VerbMophemes.InfinitiveV

  inline def parseCase: Case =
    val caseString: String = consume.toString + consume.toString + consume.toString
    skip(':')
    resolveCase(caseString)

  inline def resolveCase(str: String): Case = str.toLowerCase match {
    case a: String if a == "nom" => Nominative
    case a: String if a == "gen" => Genetive
    case a: String if a == "akk" => Akkusative
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
    case a: String if a == "kom" => Komitative
  }

  //=========================================

  inline def parseParticiple: VerbMophemes =
    skipAll("Cip:")
    peek match {
      case 'A' => skipAll("Age:"); parseParticipleAgent
      case _   => parseParticipleNormal
    }

  inline def parseParticipleNormal: VerbMophemes.Participle =
    val tempus = parseTempus
    val voice = parseVoice
    VerbMophemes.Participle(tempus, voice)

  inline def parseParticipleAgent: VerbMophemes.ParticipleAgent =
    val mode = parseMode
    VerbMophemes.ParticipleAgent(mode)
}
