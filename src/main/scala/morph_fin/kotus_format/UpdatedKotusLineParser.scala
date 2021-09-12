package morph_fin.kotus_format

import morph_fin.utils.{FilesLocation, Parser}

import scala.io.{Codec, Source}


object LoadUpdatedKotus {
  val fileName = FilesLocation.result_path + "/kotus_updated.txt"

  def apply(): Seq[UpdatedWord] = (
    for(line: String <- Source.fromFile(fileName)(Codec.UTF8).getLines)
      yield parseLine(line)
    ).toSeq

  def parseLine(str: String): UpdatedWord =
    try
      new UpdatedKotusLineParser(str.iterator).parse
    catch
      case e: Exception =>
        println(str)
        throw e
}

class UpdatedKotusLineParser(stream: Iterator[Char]) extends Parser(stream){

  def parse: UpdatedWord =
    val result = peek match {
      case 'W' => skipAll("W:"); parseWord
      case 'U' => skipAll("U:"); parsePronoun
      case 'N' => skipAll("N:"); parseNoBending
      case 'P' => skipAll("P:"); parsePrefix
      case 'S' => skipAll("S:"); parseSuffix
      case 'e' => skipAll("e:"); parseError1
      case 'E' => skipAll("E:"); parseError2
      case '1' => skipAll("1:"); parseCompound1
      case '2' => skipAll("2:"); parseCompound2
    }
    result

  inline def isEnding = peek == ':' || peek == '\n' || peek == streamEnded
  inline def isEndOfLine = peek == '\n' || peek == streamEnded

  import UpdatedWord._

  inline def parseWord: StandardBending =
    val word = collectUntil(peek == ':')
    val bending = parseBending
    StandardBending(word, bending)

  inline def parsePronoun: Pronoun =
    val word = collectUntil(isEndOfLine)
    Pronoun(word)

  inline def parseNoBending: NoInflection =
    val word = collectUntil(isEndOfLine)
    NoInflection(word)

  inline def parsePrefix: Prefix =
    val word = collectUntil(isEndOfLine)
    Prefix(word)

  inline def parseSuffix: Suffix =
    val word = collectUntil(peek == ':')
    val bending = parseBending
    Suffix(word, bending)

  inline def parseBending: Inflection =
    skipAll(":B:")
    val bendingNumber = collectUntil(isEnding).toInt
    val gradationLetterOpt: Option[Char] = peek2 match {
      case (':', 'L') =>
        skipAll(":L:")
        Some(consume)
      case _ => None
    }
    Inflection(bendingNumber, gradationLetterOpt)

  inline def parseError1: SuffixError =
    val word = collectUntil(isEndOfLine)
    SuffixError(word)

  inline def parseError2: NoInflection =
    val word = collectUntil(isEnding)
    skipUntil(isEndOfLine)
    NoInflection(word)

  inline def parseCompound1: Compound =
    val word = collectUntil(isEnding)
    skipAll(":P:")
    val prefix = collectUntil(isEnding)
    skipAll(":S:")
    val suffix = collectUntil(isEnding)
    val suffixBending = parseBending
    Compound(word, prefix, SubwordWithInflection(suffix, suffixBending))

  inline def parseCompound2: Compound2 =
    val word = collectUntil(isEnding)
    skipAll(":P:")
    val prefix = collectUntil(isEnding)
    val prefixBending = parseBending
    skipAll(":S:")
    val suffix = collectUntil(isEnding)
    val suffixBending = parseBending
    Compound2(word, SubwordWithInflection(prefix, prefixBending), SubwordWithInflection(suffix, suffixBending))
}
