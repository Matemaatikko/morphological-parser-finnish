package morph_fin.parser

import morph_fin.fsa._
import morph_fin.inflection.{Inflection, InflectionRule, InflectionsReader, Lemma, WordListReader}

case class ParsingResult(lemma: String, morphemes: String)
case class Link(key: String, lemma: String)

class FinMorphParser {

  private val words: State[Link] = {
    val words: Seq[Lemma] = WordListReader.read()
    val processed = words.map(a => a.root -> Link(a.key, a.lemma))
    FSABuilder.build(processed)
  }

  private val inflections: Map[String, State[String]] = {
    val inflections: Seq[InflectionRule] = InflectionsReader.read()

    def build(list: Seq[Inflection]): State[String] =
      val processed = list.map(a => a.ending -> a.morphemes)
      FSABuilder.build(processed)

    inflections.map(a => a.key -> build(a.list)).toMap
  }

  def parse(word: String): Seq[ParsingResult] =
    val list = words.traverse(word)
    list.flatMap(tuple => {
      val (link, rest) = tuple
      if(link.key.toLowerCase == "ind" && rest.isEmpty) Seq(ParsingResult(link.lemma, "IND"))
      else if (link.key.toLowerCase == "ind") Nil
      else
        val machine = inflections(link.key)
        val morphemesList = machine.find(rest)
        morphemesList.map(morphemes => ParsingResult(link.lemma, morphemes))
    })

}
