package morph_fin.inflection

import morph_fin.kotus_format.{LoadUpdatedKotus, PrintUpdatedWord, ReformatKotus, UpdatedWord}
import morph_fin.rulings.GradationHandler
import morph_fin.rulings.morpheme.{Morphemes, PrintMorphemes}
import morph_fin.rulings.nouns.{AllDeclensionUtils, InflectedWord, Word}
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNomineRules}
import morph_fin.utils.{FilesLocation, LongestStartingSubstring}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets


case class Ending(ending: String, morphemes: Morphemes){
  override def toString: String = ending + " " + PrintMorphemes.apply(morphemes)
}
case class EndingList(list: Set[Ending])

class EndingSeparator {

  var endingList = IndexedSeq[EndingList]()
  var rootMap = Seq[(String, Int)]()

  def parse(inflections: Seq[InflectedWord]): (String, EndingList) =
    val inflectionWords = inflections.map(_.word.toString)

    val root = LongestStartingSubstring(inflectionWords)
    val endingList = inflections.map(inflection => Ending(inflection.word.toString.drop(root.length), inflection.morphemes))

    (root, EndingList(endingList.toSet))

  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules

  val words: Seq[Word] = LoadUpdatedKotus.apply()
    .filter(_.isInstanceOf[UpdatedWord.StandardBending])
    .map(_.asInstanceOf[UpdatedWord.StandardBending])
    .filter(_.inflection.rule <= 49)
    .map(a => Word.from(a.word, a.inflection.rule, a.inflection.gradationLetter))

  val wordListFileName = FilesLocation.files_path + "/result/final-word-list.txt"
  val endingFileName = FilesLocation.files_path + "/result/inflections.txt"
  val writer = new OutputStreamWriter(new FileOutputStream(wordListFileName), StandardCharsets.UTF_8)

  def register(word: Word) =
    val declensions = AllDeclensionUtils.generateAllDeclections(word)
    val (root, endings) = parse(declensions)
    val index = endingList.indexOf(endings)
    if index == -1 then
      endingList = endingList :+ endings
      writer.write("\n" + word.lemma + "\t" + root + "\t" + (endingList.size - 1))
    else
      writer.write("\n" + word.lemma + "\t" + root + "\t" + index)

  def registerAll =
    words.foreach(register(_))
    writer.close()
    registerEndingList

  def registerEndingList =
    val writer2 = new OutputStreamWriter(new FileOutputStream(endingFileName), StandardCharsets.UTF_8)
    endingList.zipWithIndex.foreach(value => writer2.write("\n" + value._2 + "|" + value._1.list.mkString(",")))
    writer2.close()




}
