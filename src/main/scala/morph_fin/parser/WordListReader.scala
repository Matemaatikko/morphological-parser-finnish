package morph_fin.parser

import morph_fin.utils.{FilesLocation, Parser}

import scala.io.{Codec, Source}

object WordListReader {

  def read(): Seq[Lemma] = {
    val filename = FilesLocation.result_path  + "/words.txt"
    val content = for (line <- Source.fromFile(filename)(Codec.UTF8).getLines if line.nonEmpty) yield WordEntryParser(line.iterator).parse
    content.toSeq
  }
}

case class Lemma(lemma: String, root: String, key: String)

class WordEntryParser(stream: Iterator[Char]) extends Parser(stream) {

  def parse: Lemma =
    val lemma = collectUntil(peek.isWhitespace)
    skipWhiteSpaces
    val root = collectUntil(peek.isWhitespace)
    skipWhiteSpaces
    val key = collectUntil(peek.isWhitespace)
    skipWhiteSpaces
    Lemma(lemma, root, key)
}
