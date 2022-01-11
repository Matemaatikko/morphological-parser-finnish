import morph_fin.parser._

def timed[A](f: => A): A = {
  val start = System.currentTimeMillis()
  val a = f
  val end = System.currentTimeMillis()
  println("Timed: " + (end - start))
  a
}

//val words: Seq[Lemma] = timed(WordListReader.read())
//val processed = timed(words.map(a => a.root -> Link(a.key, a.lemma)))
//timed(FSABuilder.build(processed))

//val inflections: Seq[InflectionRule] = timed(InflectionsReader.read())
//
//def build(list: Seq[Inflection]): State[String] =
//  val processed = list.map(a => a.ending -> a.morphemes)
//  timed(FSABuilder.build(processed))
//
//inflections.map(a => a.key -> build(a.list)).toMap

//println(WordListReader.read().mkString("\n"))

val parser = timed(new FinMorphParser)

//parser.words.traverse("aakkoseni")
parser.parse("juoksevamme")