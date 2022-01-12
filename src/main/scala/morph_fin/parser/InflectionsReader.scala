package morph_fin.parser


import scala.io.{Codec, Source}

object InflectionsReader {
  def read(): Seq[InflectionRule] = {
    val filename = FileLocation.result_path  + "/inflections.txt"
    val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines if line.nonEmpty) yield InflectionEntryParser(line.iterator).parse
    content.toSeq.groupBy(_._1).map(tuple => InflectionRule(tuple._1, tuple._2.map(_._2))).toSeq
  }
}

case class Inflection(ending: String, morphemes: String)
case class InflectionRule(key: String, list: Seq[Inflection])

class InflectionEntryParser(stream: Iterator[Char]) extends Parser(stream) {

  def parse: (String, Inflection) =
    skipWhiteSpaces
    val key = collectUntil(peek =='\t'); skip('\t')
    val morphemes = collectUntil(peek =='\t'); skip('\t')
    val ending = collectUntil(hasEnded)
    key -> Inflection(ending, morphemes)
}