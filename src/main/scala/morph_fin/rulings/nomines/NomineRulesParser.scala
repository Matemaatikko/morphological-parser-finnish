package morph_fin.rulings.nomines

import morph_fin.rulings.*
import morph_fin.utils.FilesLocation

import scala.annotation.tailrec
import scala.io.{Codec, Source}


object LoadAndParseNomineRules {

  def apply(): Seq[NomineExampleBending] = {
    val filename = FilesLocation.rules_path  + "/nomine_rules.txt"
    val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines) yield line
    new NomineRulesParser(content.mkString("\n").iterator).parse
  }

  def rules: Seq[NomineBending] = {
    apply().map(GenerateNomineRules(_))
  }
}

case class Gradation(strong: String, weak: String)
case class NomineExampleBending(number: Int, lemma: String, gradation: Option[Gradation], cases: Seq[(NomineMorphemes, String, Boolean)])

class NomineRulesParser(stream: Iterator[Char]) {

  import Case.*
  import Form.*

  var currentCharacter: Option[Char] = Some(' ')

  inline def peek: Char =
    currentCharacter.getOrElse(throw new Exception("Failed to retreive a character from empty stream!"))

  inline def consume: Char =
    val last = peek
    if (stream.hasNext) currentCharacter = Some(stream.next)
    else currentCharacter = None
    last

  inline def skip(value: Char, error: String = ""): Unit =
    if peek != value then throw new Exception(error + peek)
    consume

  inline def skipWhiteSpaces =
    while (currentCharacter.exists(_.isWhitespace)) consume

  inline def skipWhiteSpacesUnlessLineBreak =
    while (currentCharacter.exists(a => a.isWhitespace && a != '\n'))  consume

  inline def collectUntil(condition: => Boolean): String =
    @tailrec
    def iter(result: String): String =
      if(!condition) iter(result + consume)
      else result
    iter("")

  inline def doUntil[A](fun: => A, condition: => Boolean): Seq[A] =
    @tailrec
    def iter(result: Seq[A]): Seq[A] =
      if(!condition) iter(result :+ fun)
      else result
    iter(Nil)

  def parse: Seq[NomineExampleBending] =
    skipWhiteSpaces
    skipComments
    doUntil(parseNextEntry, !currentCharacter.contains('<'))

  def skipComments: Unit =
    if(peek == '#') doUntil(consume, peek == '\n')
    skipWhiteSpaces
    if(peek == '#') skipComments

  inline def parseNextEntry: NomineExampleBending =
    skipWhiteSpaces
    skip('<')
    val number = collectUntil( !peek.isDigit).toInt
    val gradation = parseGradation
    skip('>')
    val lines = doUntil(parseLine, currentCharacter.isEmpty || currentCharacter.contains('<')).toSeq
    val lemma = lines.find(tuple => isLemma(tuple._1)).get._2.head
    val wordList = lines.flatMap(a => a._2.map(b => (a._1, b._1, b._2)))
    NomineExampleBending(number, lemma._1, gradation, wordList)

  inline def isLemma(moprhemes: NomineMorphemes): Boolean =
    moprhemes.cse == Nominative && moprhemes.form == Singular

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


  inline def parseLine: (NomineMorphemes, Seq[(String, Boolean)]) =
    skipWhiteSpaces
    val form = peek match {
      case 'S' => Singular
      case 'P' => Plural
    }
    consume
    skip('|')
    val caseString: String = consume.toString + consume.toString + consume.toString
    val cse = resolveCase(caseString)
    skip(':')
    skipWhiteSpaces
    val words = collectUntil(currentCharacter.isEmpty || currentCharacter.contains('\n')).split(' ')
    val paranthesesRemoved = words.map(word => if word.contains("(") then word.replace("(", "").replace(")", "") -> true else word -> false )
    skipWhiteSpaces
    (NomineMorphemes(cse, form), paranthesesRemoved)

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


}
