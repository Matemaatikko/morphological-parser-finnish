package morph_fin.kotus_format


import morph_fin.rulings.nomines.Gradation

import scala.annotation.tailrec

enum KotusWord(val value: String, val noPrefix: Boolean):
  case Prefix(override val value: String) extends KotusWord(value, false)
  case Suffix(override val value: String) extends KotusWord(value, true)
  case Word(override val value: String) extends KotusWord(value, true)

case class Bending(rule: Int, gradationLetter: Option[Char])
case class Entry(word: KotusWord, bending: Option[Bending])


object ParseLine{
  def apply(str: String): Seq[Entry] =
    try
      new KotusParser(str.iterator).parse
    catch
      case e: Exception =>
        println(str)
        throw e
}

class KotusParser(stream: Iterator[Char]) {

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

  def skipUntil(value: Char, error: String = ""): Unit =
    if peek != value then
      consume
      skipUntil(value, error)


  def skipAll(value: String, error: String = ""): Unit =
    value.foreach(c => skip(c))

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

  def parse: Seq[Entry] =
    skipWhiteSpaces
    skipAll("<st>")
    skipAll("<s>")
    val word = collectUntil(peek == '<')
    skipAll("</s>")
    skip('<')
    if(peek == 'h')
      skipAll("hn>")
      consume
      skipAll("</hn>")
      skip('<')
    val bendingList = parseBendingList
    val kotusWord =
      if word.startsWith("-") then KotusWord.Suffix(word.drop(1))
      else if word.endsWith("-") then KotusWord.Prefix(word.dropRight(1))
      else KotusWord.Word(word)
    if bendingList.isEmpty then Seq(Entry(kotusWord, None))
    else bendingList.map(bending => Entry(kotusWord, Some(bending)))

  def parseBendingList: Seq[Bending] =
    @tailrec
    def iter(result: Seq[Bending] = Nil): Seq[Bending] =
      if(peek == 't')
        val bending = parseBending
        skip('<')
        iter(result ++ bending)
      else result

    iter(Nil)

  def parseBending: Seq[Bending] =
    skip('t')
    skipUntil('>') //Possible info skipped
    skip('>')
    skipAll("<tn>")
    val number = collectUntil(peek == '<').toInt
    skipAll("</tn>")
    skip('<')
    val (gradation, optional) = if(peek == 'a')
      skipAll("av")
      val value = collectUntil(peek == '>') //Possible info skipped
      val opt = value.trim == "astevaihtelu=\"valinnainen\""
      skip('>')
      val res = consume
      skipAll("</av>")
      skip('<')
      Some(res) -> opt
    else None -> false
    skipAll("/t>")
    if(optional) Seq(Bending(number, gradation), Bending(number, None)) else Seq(Bending(number, gradation))

}
