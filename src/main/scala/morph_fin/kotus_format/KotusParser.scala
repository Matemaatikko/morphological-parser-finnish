package morph_fin.kotus_format

import morph_fin.rulings.rules.Gradation
import morph_fin.utils.{FilesLocation, Parser}

import scala.annotation.tailrec
import scala.io.{Codec, Source}

object ParseKotus {
  val fileName = FilesLocation.files_path + "/kotus-sanalista_v1_original.xml"

  def apply(): Seq[Entry] = (
    for(line: String <- Source.fromFile(fileName)(Codec.UTF8).getLines)
      yield
        if(line.startsWith("<st>")) parseLine(line)
        else Nil
    ).flatten.toSeq

  def parseLine(str: String): Seq[Entry] =
    try
      new KotusParser(str.iterator).parse
    catch
      case e: Exception =>
        println(str)
        throw e
}


enum KotusWord(val value: String, val noPrefix: Boolean):
  case Prefix(override val value: String) extends KotusWord(value, false)
  case Suffix(override val value: String) extends KotusWord(value, true)
  case Word(override val value: String) extends KotusWord(value, true)

case class Inflection(rule: Int, gradationLetter: Option[Char])
case class Entry(word: KotusWord, inflectionOpt: Option[Inflection])

class KotusParser(stream: Iterator[Char]) extends Parser(stream){

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

  def parseBendingList: Seq[Inflection] =
    @tailrec
    def iter(result: Seq[Inflection] = Nil): Seq[Inflection] =
      if(peek == 't')
        val bending = parseBending
        skip('<')
        iter(result ++ bending)
      else result

    iter(Nil)

  def parseBending: Seq[Inflection] =
    skip('t')
    skipUntil(peek == '>') //Possible info skipped
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
    if(optional) Seq(Inflection(number, gradation), Inflection(number, None)) else Seq(Inflection(number, gradation))

}
