package morph_fin.rulings

import scala.annotation.tailrec

enum Case:
  case Nominative, Genetive, Akkusative, Partitive
  case Inessive, Elative, Illative
  case Adessive, Ablative, Allative
  case Essive, Translative, Instructive, Abessive, Komitative
end Case

enum Type:
  case Plural, Singular

enum GradationType:
  case Strong, Weak, Unnecessary

case class Gradation(strong: String, weak: String, gradationType: GradationType = GradationType.Unnecessary)
case class Ruling(number: Int, lemma: String, gradation: Option[Gradation], cases: Seq[(Case, Type, String)])

class NomineRulesParser(stream: Iterator[Char]) {

  import Case._
  import Type._

  var currentCharacter: Option[Char] = Some(' ')

  def peek: Char =
    currentCharacter.getOrElse(throw new Exception("Failed to retreive a character from empty stream!"))

  def consume: Char =
    val last = peek
    if (stream.hasNext) currentCharacter = Some(stream.next)
    else currentCharacter = None
    last

  def skip(value: Char, error: String = ""): Unit =
    if peek != value then throw new Exception(error + peek)
    consume

  def skipWhiteSpaces =
    while (currentCharacter.exists(_.isWhitespace)) consume

  def skipWhiteSpacesUnlessLineBreak =
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

  def parse: Seq[Ruling] =
    skipWhiteSpaces
    skipComments
    doUntil(parseNextEntry, !currentCharacter.contains('<'))

  def skipComments: Unit =
    if(peek == '#') doUntil(consume, peek == '\n')
    skipWhiteSpaces
    if(peek == '#') skipComments

  def parseNextEntry: Ruling =
    skipWhiteSpaces
    skip('<')
    val number = collectUntil( !peek.isDigit).toInt
    val gradation =
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
        Some(Gradation(strong, weak, tpe))
      else None
    skip('>')
    val lines = doUntil(parseLine, currentCharacter.isEmpty || currentCharacter.contains('<')).toSeq
    val lemma = lines.find(value => value._1 == Nominative && value._2 == Singular).get._3.head
    val wordList = lines.flatMap(a => a._3.map(b => (a._1, a._2, b)))
    Ruling(number, lemma, gradation, wordList)

  def parseLine: (Case, Type, Seq[String]) =
    skipWhiteSpaces
    val _type = peek match {
      case 'S' => Singular
      case 'P' => Plural
    }
    consume
    skip('|')
    val caseString: String = consume.toString + consume.toString + consume.toString
    val _case = resolveCase(caseString)
    skip(':')
    skipWhiteSpaces
    val words = collectUntil(currentCharacter.isEmpty || currentCharacter.contains('\n')).split(' ')
    val paranthesesRemoved = words.map(_.replace("(", "").replace(")", ""))
    skipWhiteSpaces
    (_case, _type, paranthesesRemoved)

  def resolveCase(str: String): Case = str.toLowerCase match {
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
