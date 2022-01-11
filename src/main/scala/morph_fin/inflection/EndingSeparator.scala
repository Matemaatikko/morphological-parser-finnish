package morph_fin.inflection

import morph_fin.kotus_format.{LoadUpdatedKotus, PrintUpdatedWord, ReformatKotus, UpdatedWord}
import morph_fin.rulings.GradationHandler
import morph_fin.rulings.morpheme.{Morphemes, PrintMorphemes}
import morph_fin.rulings.nouns.{AllDeclensionUtils, InflectedWord, Word}
import morph_fin.rulings.pronouns.{LoadPronounInflections, PronounIflection}
import morph_fin.rulings.rules.{ConjugationRule, DeclensionRule, LoadAndParseNounRules, LoadAndParseVerbRules}
import morph_fin.rulings.verbs.AllConjugationUtils
import morph_fin.utils.{FilesLocation, LongestStartingSubstring}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets


case class Ending(ending: String, morphemes: Morphemes){
  override def toString: String = ending + " " + PrintMorphemes.apply(morphemes)
}
case class EndingList(list: Set[Ending])

class EndingSeparator {

  var nounEndings = IndexedSeq[EndingList]()
  var verbEndings = IndexedSeq[EndingList]()
  var rootMap = Seq[(String, Int)]()

  def parse(inflections: Seq[InflectedWord]): (String, EndingList) =
    val inflectionWords = inflections.map(_.word.toString)

    val root = LongestStartingSubstring(inflectionWords)
    val endingList = inflections.map(inflection => Ending(inflection.word.toString.drop(root.length), inflection.morphemes))

    (root, EndingList(endingList.toSet))

  given Seq[DeclensionRule] = LoadAndParseNounRules.rules
  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules

  val allWords = LoadUpdatedKotus.apply()

  val compoundWords = allWords.filter(_.isInstanceOf[UpdatedWord.Compound2])
    .map(_.asInstanceOf[UpdatedWord.Compound2])

  val indeclinables: Seq[String] = allWords.filter(_.isInstanceOf[UpdatedWord.NoInflection])
    .map(_.asInstanceOf[UpdatedWord.NoInflection])
    .map(_.word)

  val standardInflection: Seq[Word] = allWords.filter(_.isInstanceOf[UpdatedWord.StandardBending])
    .map(_.asInstanceOf[UpdatedWord.StandardBending])
    .map(a => Word.from(a.word, a.inflection.rule, a.inflection.gradationLetter))

  val pronouns: Seq[PronounIflection] = LoadPronounInflections.apply()
  val nouns = standardInflection.filter(_.ruleNumber <= 49)
  val verbs = standardInflection.filter(_.ruleNumber >= 52)

  val wordListFileName = FilesLocation.files_path + "/result/words.txt"
  val endingFileName = FilesLocation.files_path + "/result/inflections.txt"
  val writer = new OutputStreamWriter(new FileOutputStream(wordListFileName), StandardCharsets.UTF_8)

  //NOUNS

  def registerNoun(word: Word) =
    val declensions = AllDeclensionUtils.generateAllDeclections(word)
    val (root, endings) = parse(declensions)
    val index = nounEndings.indexOf(endings)
    if index == -1 then
      nounEndings = nounEndings :+ endings
      writer.write("\n" + word.lemma + "\t" + root + "\tN" + (nounEndings.size - 1))
    else
      writer.write("\n" + word.lemma + "\t" + root + "\tN" + index)

  def registerPronoun(pronounIflection: PronounIflection) =
    val (root, endings) = parse(pronounIflection.inflections)
    val index = nounEndings.indexOf(endings)
    if index == -1 then
      nounEndings = nounEndings :+ endings
      writer.write("\n" + pronounIflection.lemma + "\t" + root + "\tN" + (nounEndings.size - 1))
    else
      writer.write("\n" + pronounIflection.lemma + "\t" + root + "\tN" + index)

  def registerCompound(word: UpdatedWord.Compound2) =
    val declensions = AllDeclensionUtils.generateDeclectionsForCompound(word.word, word.prefix, word.suffix)
    val (root, endings) = parse(declensions)
    val index = nounEndings.indexOf(endings)
    if index == -1 then
      nounEndings = nounEndings :+ endings
      writer.write("\n" + word.word + "\t" + root + "\tN" + (nounEndings.size - 1))
    else
      writer.write("\n" + word.word + "\t" + root + "\tN" + index)

  //VERB

  def registerVerb(word: Word) =
    val conjugations = AllConjugationUtils.generateAllConjugations(word)
    val (root, endings) = parse(conjugations)
    val index = verbEndings.indexOf(endings)
    if index == -1 then
      verbEndings = verbEndings :+ endings
      writer.write("\n" + word.lemma + "\t" + root + "\tV" + (verbEndings.size - 1))
    else
      writer.write("\n" + word.lemma + "\t" + root + "\tV" + index)

  //INDECLINABLES

  def registerIndeclinable(word: String) =
      writer.write("\n" + word + "\t" + word + "\tIND")

  //ALL

  def exec[A](list: Seq[A], f : A => Unit): Unit =
    val size: Double = list.size
    list.zipWithIndex.foreach((a, ind) => {
      f(a)
      if(ind % 200 == 0)
        writer.flush()
        print("  "+ (((ind / size)*1000).toInt / 10.0) + "%")
    })

  def registerAll =
    exec(nouns, registerNoun(_))
    println("\nNouns Done!")
    exec(compoundWords, registerCompound(_))
    println("\nCompounds Done!")
    exec(pronouns, registerPronoun(_))
    println("\nPronouns Done!")
    exec(verbs, registerVerb(_))
    println("\nVerbs Done!")
    exec(indeclinables, registerIndeclinable(_))
    println("\nIndeclinables Done!")
    writer.close()
    registerEndings

  def processEndings(endings: Seq[EndingList]): Seq[(String, String, String)] =
    endings.zipWithIndex.flatMap(tuple => tuple._1.list.map(ending => (tuple._2.toString, ending.ending, PrintMorphemes.apply(ending.morphemes))))

  def registerEndings =
    val writer2 = new OutputStreamWriter(new FileOutputStream(endingFileName), StandardCharsets.UTF_8)
    processEndings(nounEndings).foreach(triple => writer2.write("\nN" + triple._1 + "\t" + triple._3 + "\t" + triple._2))
    writer2.flush()
    processEndings(verbEndings).foreach(triple => writer2.write("\nV" + triple._1 + "\t" + triple._3 + "\t" + triple._2))
    writer2.close()
}

object FinalFormatGenerator extends App {

  new EndingSeparator().registerAll

}
