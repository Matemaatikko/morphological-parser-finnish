package morph_fin.inflection

import morph_fin.kotus_format.UpdatedWord.{Compound, Compound2, StandardBending}
import morph_fin.kotus_format.{Bending, LoadUpdatedKotus, UpdatedWord}
import morph_fin.rulings.nomines.{DeclensionUtils, Gradation, LoadAndParseNomineRules, PossessiveSuffixGeneration, ResultWord, Word}
import morph_fin.rulings.verbs.{ConjugationUtils, LoadAndParseVerbRules}
import morph_fin.rulings.{FilePrint, GradationHandler, Morphemes, NomineMorphemes}
import morph_fin.utils.{FilesLocation, Letters}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

enum TargetFile:
  case Verb, Noun, Compound, Indeclinable, Pronoun, Ignore

object GenerateInflectedWords {
  val nomineBendings = LoadAndParseNomineRules.rules
  val verbBendings = LoadAndParseVerbRules.rules

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
      case a: NoBending => TargetFile.Indeclinable
      case a: Error => TargetFile.Indeclinable
      case _ => TargetFile.Ignore
    }

  def getWriter(targetFile: TargetFile): OutputStreamWriter =
    import TargetFile._
    val str: String = targetFile match {
      case Verb => verb
      case Noun => noun
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
        val resulWords = getBendingsWithoutSuffix(suffixWord.value, suffixWord.bending)
        resulWords.flatMap(resultWord => {
          val gradationOpt = suffixWord.bending.gradationLetter.map(GradationHandler.getGradationByLetter(_))
          if resultWord.morphemes.isInstanceOf[NomineMorphemes] then
            addPossessiveSuffixes(word, prefix, resultWord, gradationOpt)
          else {
            val result = addHyphenIfNeeded(prefix, resultWord.word.toString, word)
            Seq(print(result, resultWord.morphemes, word))
          }
        })
      case Compound2(word, prefixWord, suffixWord) =>
        val prefixBendings = getBendingsWithoutSuffix(prefixWord.value, prefixWord.bending)
        val suffixBendings = getBendingsWithoutSuffix(suffixWord.value, suffixWord.bending)
        val morphemes = prefixBendings.map(_.morphemes).distinct
        morphemes.flatMap(morphemes => {
          val prefixBending = prefixBendings.find(_.morphemes == morphemes).get
          val suffixBending = suffixBendings.find(_.morphemes == morphemes).get
          val gradationOpt = suffixWord.bending.gradationLetter.map(GradationHandler.getGradationByLetter(_))
          addPossessiveSuffixes(word, prefixBending.word.toString, suffixBending, gradationOpt)
        })
      case StandardBending(word, bending) =>
        val bendings = getBendingsWithSuffix(word, bending)
        bendings.map(result => print(result.word.toString, result.morphemes, result.lemma))
      case Pronoun(value) => Seq(value + "\n")
      case NoBending(value) => Seq(value+ "\n")
      case Error(value, _, _) => Seq(value+ "\n")
      case _ => Nil
    }

  def addPossessiveSuffixes(word: String, prefix: String, suffix: ResultWord, gradationOpt: Option[Gradation]): Seq[String] =
    val suffixInflectionsWithPosSuffixes = PossessiveSuffixGeneration.addSuffixes(suffix, gradationOpt)
    suffixInflectionsWithPosSuffixes.map(suffix => {
      val resultWord = addHyphenIfNeeded(prefix, suffix.word.toString, word)
      print(resultWord, suffix.morphemes, word)
    })

  def addHyphenIfNeeded(prefix: String, suffix: String, lemma: String): String =
    if(prefix.length == 1 || lemma.contains('-')) prefix + "-" + suffix
    else if(Letters.isVowel(prefix.last) && prefix.last == suffix.head) prefix + "-" + suffix
    else prefix + suffix

  def print(word: String, morphemes: Morphemes, lemma: String): String =
    fill(word, 40) + ":" + fill(FilePrint(morphemes)+ ":", 25)  + lemma + "\n"

  def fill(word: String, upTo: Int): String =
    val num = upTo - word.length
    if(num > 0) word + " "*num
    else word

  def getBendingsWithSuffix(lemma: String, bending: Bending) =
    val word =  Word(lemma, bending.rule, bending.gradationLetter.map(GradationHandler.getGradationByLetter(_)))
    if(bending.rule < 50) genDeclensionsWithSuffixes(word)
    else if(bending.rule > 51 && bending.rule < 79) ConjugationUtils.generateConjugations(verbBendings, word)
    else throw new Exception()

  def getBendingsWithoutSuffix(lemma: String, bending: Bending) =
    val word =  Word(lemma, bending.rule, bending.gradationLetter.map(GradationHandler.getGradationByLetter(_)))
    if(bending.rule < 50) DeclensionUtils.generateDeclensions(nomineBendings, word)
    else if(bending.rule > 51 && bending.rule < 79) ConjugationUtils.generateConjugations(verbBendings, word)
    else throw new Exception()


  def genDeclensionsWithSuffixes(word: Word) =
    DeclensionUtils.generateDeclensions(nomineBendings, word)
      .flatMap(a => handle(a, word.gradation))

  import morph_fin.rulings.MorphemesUtils._
  import morph_fin.rulings.PossessiveSuffix

  def handle(resultWord: ResultWord, gradationOpt: Option[Gradation]): Seq[ResultWord] =
    if PossessiveSuffixGeneration.isSuitableForSuffix(resultWord.morphemes) then
      val nonVnBody =  PossessiveSuffixGeneration.getRootForNonVnSuffixes(resultWord, gradationOpt)
      val vn = PossessiveSuffixGeneration.addVnSuffix(nonVnBody, gradationOpt)

      val nonVn = ResultWord(nonVnBody.word.append("-P"), nonVnBody.morphemes ++ PossessiveSuffix.Body, nonVnBody.lemma)
      Seq(resultWord, nonVn) ++ vn
    else Seq(resultWord)
}
