package morph_fin.inflection

import morph_fin.kotus_format.UpdatedWord.{Compound, Compound2, StandardBending}
import morph_fin.kotus_format.{Bending, LoadUpdatedKotus, UpdatedWord}
import morph_fin.rulings.nomines.{GenerateDeclensionWords, LoadAndParseNomineRules, Word}
import morph_fin.rulings.verbs.{GenerateConjugatedWords, LoadAndParseVerbRules}
import morph_fin.rulings.{FilePrint, GradationHandler, Morphemes}
import morph_fin.utils.{FilesLocation, Letters}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

object GenerateInflectedWords {
  val nomineBendings = LoadAndParseNomineRules.rules
  val verbBendings = LoadAndParseVerbRules.rules

  val verb = FilesLocation.result_path + s"/bendings/verbs.txt"
  val noun = FilesLocation.result_path + s"/bendings/nouns.txt"
  val compound = FilesLocation.result_path + s"/bendings/compounds.txt"
  val pronoun = FilesLocation.result_path + s"/bendings/pronoun.txt"
  val indeclinable = FilesLocation.result_path + s"/bendings/indeclinables.txt"

  enum TargetFile:
    case Verb, Noun, Compound, Indeclinable, Pronoun, Ignore

  def apply: Unit =
    val words: Seq[UpdatedWord] = LoadUpdatedKotus.apply()
    val generated = words.map(tryGenerate(_))
    generated.groupBy(_._2)
      .toSeq
      .map(tuple => tuple._1 -> tuple._2.flatMap(_._1))
      .map(tuple => writeAll(tuple._1, tuple._2))

  def writeAll(targetFile: TargetFile, words: Seq[String]): Unit =
    import TargetFile._
    val file: Option[String] = targetFile match {
      case Verb => Some(verb)
      case Noun => Some(noun)
      case TargetFile.Compound => Some(compound)
      case Indeclinable => Some(indeclinable)
      case Pronoun => Some(pronoun)
      case Ignore => None
    }
    file match {
      case Some(str) =>
        val writer = new OutputStreamWriter(new FileOutputStream(str), StandardCharsets.UTF_8)
        words.foreach(writer.write(_))
        writer.close()
      case None =>
    }


  def tryGenerate(updatedWord: UpdatedWord): (Seq[String], TargetFile) =
    try generate(updatedWord) catch
      case e: Exception =>
        println("Exception raised: " + updatedWord.toString)
        e.printStackTrace()
        Nil -> TargetFile.Ignore

  def generate(updatedWord: UpdatedWord): (Seq[String], TargetFile) =
    import UpdatedWord._
    updatedWord match {
      case Compound(word, prefix, suffixWord) =>
        val bendings = getBendings(suffixWord.value, suffixWord.bending)
        bendings.map(result => {
          val resultWord = addHyphenIfNeeded(prefix, result.word.toString)
          print(resultWord, result.morphemes, word)
        }) -> TargetFile.Compound
      case Compound2(word, prefixWord, suffixWord) =>
        val prefixBendings = getBendings(prefixWord.value, prefixWord.bending)
        val suffixBendings = getBendings(suffixWord.value, suffixWord.bending)
        val morphemes = prefixBendings.map(_.morphemes).distinct
        morphemes.map(morphemes => {
          val prefixBending = prefixBendings.find(_.morphemes == morphemes).get
          val suffixBending = suffixBendings.find(_.morphemes == morphemes).get
          val resultWord = addHyphenIfNeeded(prefixBending.word.toString, suffixBending.word.toString)
          print(resultWord, morphemes, word)
        })-> TargetFile.Compound
      case StandardBending(word, bending) =>
        val bendings = getBendings(word, bending)
        val target = if(bending.rule < 50) TargetFile.Noun else TargetFile.Verb
        bendings.map(result => print(result.word.toString, result.morphemes, result.lemma)) -> target
      case Pronoun(value) => Seq(value + "\n") -> TargetFile.Pronoun
      case NoBending(value) => Seq(value+ "\n") -> TargetFile.Indeclinable
      case Error(value, _, _) => Seq(value+ "\n") -> TargetFile.Indeclinable
      case _ => Nil -> TargetFile.Ignore
    }

  def addHyphenIfNeeded(prefix: String, suffix: String): String =
    if(prefix.length == 1) prefix + "-" + suffix
    else if(Letters.isVowel(prefix.last) && prefix.last == suffix.head) prefix + "-" + suffix
    else prefix + suffix

  def print(word: String, morphemes: Morphemes, lemma: String): String =
    fill(word, 40) + ":" + fill(FilePrint(morphemes)+ ":", 25)  + lemma + "\n"

  def fill(word: String, upTo: Int): String =
    val num = upTo - word.length
    if(num > 0) word + " "*num
    else word

  def getBendings(lemma: String, bending: Bending) =
    val word =  Word(lemma, bending.rule, bending.gradationLetter.map(GradationHandler.getGradationByLetter(_)))
    if(bending.rule < 50) GenerateDeclensionWords(nomineBendings, word)
    else if(bending.rule > 51 && bending.rule < 79) GenerateConjugatedWords(verbBendings, word)
    else throw new Exception()

}
