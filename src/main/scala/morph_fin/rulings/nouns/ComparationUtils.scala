package morph_fin.rulings.nouns

import morph_fin.rulings._
import morph_fin.utils.{Hyphenation, Letters}


//TODO Add tests
/**
* aito aidon/aidompi aidoin
* halpa halvan/halvempi (a -> e) halvin
* kaunis kauniin/kauniimpi kaunein
* sininen sininen/sinisempi sinisin
* juokseva juoksevan/juoksevampi juoksevin
*
*
* kiva -> kivan -> kivampi - kivempi
*      -> kivan -> kivoin / kivain
*/
object ComparationUtils {

  //COMPARATIVE

  def generateComparativeInflections(singularGenitiveWord: InflectedWord)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    assert(singularGenitiveWord.morphemes.is(Singular, Genitive))
    val comparativeWords = generateComparativeWord(singularGenitiveWord)
    comparativeWords.flatMap(a => addComparativeDeclensions(a, singularGenitiveWord.lemma))

  private def addComparativeDeclensions(comparativeForm: String, lemma: String)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val word_ = Word(comparativeForm, 16, Option(Gradation("mp", "mm")))
    val declensions = DeclensionUtils.generateDeclensions(word_)
    declensions.map(declension => declension.copy(
      lemma = lemma,
      morphemes = declension.morphemes ~ Comparative
    ))

  def generateComparativeWord(singularGenitiveWord: InflectedWord): Seq[String] =
    import singularGenitiveWord._
    val exceptions = comparativeHandleExceptionWords(lemma)
    if exceptions.nonEmpty then exceptions
    else formatForComparative(word.toString)

  private def formatForComparative(word: String): Seq[String] =
    if(has_e_asBodyVowel(word))
    then Seq(word.dropRight(2) + "empi")
    else Seq(word.dropRight(1) + "mpi")

  private def comparativeHandleExceptionWords(lemma: String): Seq[String] =
    lemma match {
      case "kiva" => Seq("kivempi", "kivampi")
      case "liila" => Seq("liilempi", "liilampi")
      case "metka" => Seq("metkempi", "metkampi")
      case "risa" => Seq("risempi", "risampi")
      case "pitkä" => Seq("pitempi")
      case "lyhyt" => Seq("lyhempi")
      case "hyvä" => Seq("parempi")
      case _ => Nil
    }

  private inline def has_e_asBodyVowel(word: String) =
    Hyphenation.apply(word).size == 2 && word.endsWith("a")

  //===================================================


  //SUPERLATIVE

  def generateSuperlativeInflections(singularGenitiveWord: InflectedWord)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    assert(singularGenitiveWord.morphemes.is(Singular, Genitive))
    val superlativeWords = generateSuperlativeWord(singularGenitiveWord)
    superlativeWords.flatMap(a => addSuperlativeDeclensions(a, singularGenitiveWord.lemma))

  private def addSuperlativeDeclensions(superlativeForm: String, lemma: String)(using rules: Seq[DeclensionRule]): Seq[InflectedWord] =
    val ruleNumber = exceptionRuleNumber(superlativeForm).getOrElse(36)
    val word_ = Word(superlativeForm, ruleNumber, None)
    val declensions = DeclensionUtils.generateDeclensions(word_)
    declensions.map(declension => declension.copy(
      lemma = lemma,
      morphemes = declension.morphemes ~ Superlative
    ))

  def exceptionRuleNumber(superlativeForm: String): Option[Int] =
    superlativeForm match {
      case "paras" => Some(41)
      case _       => None
    }

  def generateSuperlativeWord(singularGenitiveWord: InflectedWord): Seq[String] =
    import singularGenitiveWord._
    val exeptions = superlativeHandleExceptionWords(lemma)
    if exeptions.nonEmpty then exeptions
    else formatForSuperlative(word.toString.dropRight(1))

  private def formatForSuperlative(word: String): Seq[String] =
    val last2 = word.takeRight(2)
    (last2(0), last2(1)) match {
      case (a, 'e') if Letters.isConsonant(a)       => Seq(word.dropRight(1) + "in")
      case (a, 'a') if Letters.isConsonant(a)       => Seq(word.dropRight(1) + "in", word.dropRight(1) + "oin")
      case (a, 'ä') if Letters.isConsonant(a)       => Seq(word.dropRight(1) + "in", word.dropRight(1) + "öin")
      case ('i', 'i')                               => Seq(word.dropRight(2) + "ein")
      case (a, b) if Letters.isVowel(a) && a == b   => Seq(word.dropRight(1) + "in")
      case (a, 'i') if a != 'i'                     => Seq(word.dropRight(1) + "ein")
      case _                                        => Seq(word + "in")
    }

  private def superlativeHandleExceptionWords(lemma: String): Seq[String] =
    lemma match {
      case "uusi" =>  Seq("uusin")
      case "tosi" =>  Seq("tosin")
      case "täysi" => Seq("täysin")
      case "pitkä" => Seq("pisin")
      case "lyhyt" => Seq("lyhin")
      case "hyvä" =>  Seq("paras")
      case _ => Nil
    }
}


