package morph_fin.inflection

import morph_fin.kotus_format.UpdatedWord.{Compound, Compound2, StandardBending}
import morph_fin.kotus_format.{Inflection, LoadUpdatedKotus, UpdatedWord}
import morph_fin.rulings.nouns.{DeclensionUtils, InflectedWord, PossessiveSuffixUtils, Word}
import morph_fin.rulings.verbs.ConjugationUtils
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Morphemes, Noun, PrintMorphemes}
import morph_fin.rulings.rules.{ConjugationRule, DeclensionRule, Gradation, LoadAndParseNomineRules, LoadAndParseVerbRules}
import morph_fin.utils.{FilesLocation, Letters}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

enum TargetFile:
  case Verb, Noun, Compound, Indeclinable, Pronoun, Ignore

object GenerateInflectedWords {

  import morph_fin.rulings.morpheme.PossessiveSuffix

  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules
  given Seq[ConjugationRule] = LoadAndParseVerbRules.rules

  val verb = FilesLocation.result_path + s"/inflections/verbs.txt"
  val noun = FilesLocation.result_path + s"/inflections/nouns.txt"
  val compound = FilesLocation.result_path + s"/inflections/compounds.txt"
  val pronoun = FilesLocation.result_path + s"/inflections/pronoun.txt"
  val indeclinable = FilesLocation.result_path + s"/inflections/indeclinables.txt"
  val error = FilesLocation.result_path + s"/inflections/error.txt"

  def apply(filter: TargetFile): Unit =
    val words: Seq[UpdatedWord] = LoadUpdatedKotus.apply().filter(a => getTarget(a) == filter)
    val writer = getWriter(filter)
    val generated = words.foreach(word => {
      val result = tryGenerate(word)
      result.foreach(a => writer.write(a))
      writer.flush()
    })
    writer.close()


  def getTarget(updatedWord: UpdatedWord): TargetFile =
    import UpdatedWord._
    updatedWord match {
      case a: Compound => TargetFile.Compound
      case a: Compound2 => TargetFile.Compound
      case StandardBending(_, bending) if bending.rule < 50 => TargetFile.Noun
      case StandardBending(_, bending) if bending.rule > 51 && bending.rule < 79 => TargetFile.Verb
      case a: Pronoun => TargetFile.Pronoun
      case a: NoInflection => TargetFile.Indeclinable
      case a: Error => TargetFile.Indeclinable
      case _ => TargetFile.Ignore
    }

  def getWriter(targetFile: TargetFile): OutputStreamWriter =
    import TargetFile._
    val str: String = targetFile match {
      case Verb => verb
      case TargetFile.Noun => noun
      case TargetFile.Compound => compound
      case Indeclinable => indeclinable
      case Pronoun => pronoun
      case _    => error
    }
    new OutputStreamWriter(new FileOutputStream(str), StandardCharsets.UTF_8)


  def tryGenerate(updatedWord: UpdatedWord): Seq[String] =
    try generate(updatedWord) catch
      case e: Exception =>
        println("Exception raised: " + updatedWord.toString)
        e.printStackTrace()
        Nil

  def generate(updatedWord: UpdatedWord): Seq[String] =
    import UpdatedWord._
    updatedWord match {
      case Compound(word, prefix, suffixWord) =>
        val resulWords = getBendingsWithoutSuffix(suffixWord.subword, suffixWord.inflection)
        resulWords.flatMap(resultWord => {
          val gradationOpt = suffixWord.inflection.gradationLetter.map(GradationHandler.getGradationByLetter(_))
          if resultWord.morphemes.root == Noun then
            addPossessiveSuffixes(word, prefix, resultWord)
          else {
            val result = addHyphenIfNeeded(prefix, resultWord.word.toString, word)
            Seq(print(result, resultWord.morphemes, word))
          }
        })
      case Compound2(word, prefixWord, suffixWord) =>
        val prefixBendings = getBendingsWithoutSuffix(prefixWord.subword, prefixWord.inflection)
        val suffixBendings = getBendingsWithoutSuffix(suffixWord.subword, suffixWord.inflection)
        val morphemes = prefixBendings.map(_.morphemes).distinct
        morphemes.flatMap(morphemes => {
          val prefixBending = prefixBendings.find(_.morphemes == morphemes).get
          val suffixBending = suffixBendings.find(_.morphemes == morphemes).get
          val gradationOpt = suffixWord.inflection.gradationLetter.map(GradationHandler.getGradationByLetter(_))
          addPossessiveSuffixes(word, prefixBending.word.toString, suffixBending)
        })
      case StandardBending(word, bending) =>
        val bendings = getBendingsWithSuffix(word, bending)
        bendings.map(result => print(result.word.toString, result.morphemes, result.lemma))
      case Pronoun(value) => Seq(value + "\n")
      case NoInflection(value) => Seq(value+ "\n")
      case Error(value, _, _) => Seq(value+ "\n")
      case _ => Nil
    }

  def addPossessiveSuffixes(word: String, prefix: String, suffix: InflectedWord): Seq[String] =
    val suffixInflectionsWithPosSuffixes = PossessiveSuffixUtils.addSuffixes(suffix)
    suffixInflectionsWithPosSuffixes.map(suffix => {
      val resultWord = addHyphenIfNeeded(prefix, suffix.word.toString, word)
      print(resultWord, suffix.morphemes, word)
    })

  def addHyphenIfNeeded(prefix: String, suffix: String, lemma: String): String =
    if(prefix.length == 1 || lemma.contains('-')) prefix + "-" + suffix
    else if(Letters.isVowel(prefix.last) && prefix.last == suffix.head) prefix + "-" + suffix
    else prefix + suffix

  def print(word: String, morphemes: Morphemes, lemma: String): String =
    fill(word, 40) + ":" + fill(PrintMorphemes(morphemes)+ ":", 25)  + lemma + "\n"

  def fill(word: String, upTo: Int): String =
    val num = upTo - word.length
    if(num > 0) word + " "*num
    else word

  def getBendingsWithSuffix(lemma: String, bending: Inflection) =
    val word =  Word(lemma, bending.rule, bending.gradationLetter.map(GradationHandler.getGradationByLetter(_)))
    if(bending.rule < 50) genDeclensionsWithSuffixes(word)
    else if(bending.rule > 51 && bending.rule < 79) ConjugationUtils.generateConjugations(word)
    else throw new Exception()

  def getBendingsWithoutSuffix(lemma: String, bending: Inflection) =
    val word =  Word(lemma, bending.rule, bending.gradationLetter.map(GradationHandler.getGradationByLetter(_)))
    if(bending.rule < 50) DeclensionUtils.generateDeclensions(word)
    else if(bending.rule > 51 && bending.rule < 79) ConjugationUtils.generateConjugations(word)
    else throw new Exception()


  def genDeclensionsWithSuffixes(word: Word) =
    DeclensionUtils.generateDeclensions(word)
      .flatMap(a => handle(a, word.gradationOpt))

  def handle(resultWord: InflectedWord, gradationOpt: Option[Gradation]): Seq[InflectedWord] =
    if PossessiveSuffixUtils.isSuitableForSuffix(resultWord.morphemes) then
      val nonVnBody =  PossessiveSuffixUtils.getRootForNonVnSuffixes(resultWord, gradationOpt)
      val vn = PossessiveSuffixUtils.addVnSuffix(nonVnBody, gradationOpt)

      val nonVn = InflectedWord(nonVnBody.word.append("-P"), nonVnBody.morphemes ~ PossessiveSuffix.Body, nonVnBody.lemma)
      Seq(resultWord, nonVn) ++ vn
    else Seq(resultWord)
}
