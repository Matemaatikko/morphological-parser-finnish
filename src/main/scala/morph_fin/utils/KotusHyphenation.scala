package morph_fin.utils

import morph_fin.kotus_format.{Entry, ParseLine}

import scala.io.{Codec, Source}

val fileName = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish\\src\\main\\files\\kotus-sanalista_v1_original.xml"

object KotusHyphenation {
  def apply(): Seq[String] =
    val list: Seq[Entry] = (for(line: String <- Source.fromFile(fileName)(Codec.UTF8).getLines) yield
      if(line.startsWith("<st>")) Some(ParseLine(line)) else None).flatten.toSeq
    list.map(_.word.value).map(Hyphenation(_)).map(_.last).distinct


/*
  def save(list: Seq[UpdatedWord]): Unit =
    import java.io._
    import java.nio.charset.StandardCharsets
    //val str: String = list.map(PrintUpdatedWord(_)).mkString("\n")
    val writer = new OutputStreamWriter(new FileOutputStream(updatedFileName), StandardCharsets.UTF_8)
    list.foreach(value => writer.write(PrintUpdatedWord(value) + "\n"))
    writer.close()*/
}
