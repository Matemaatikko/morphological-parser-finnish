package morph_fin.rulings.nomines

import morph_fin.rulings.{ComparationMorphemes, Comparative, NomineMorphemes}
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
*/
object ComparationUtils {

  //COMPARATIVE

  def generateComparativeForms(SingularGenitiveWord: ResultWord)(using rules: Seq[DeclensionRule]): Seq[ResultWord] =
    val comparativeWords = generateComparativeWord(SingularGenitiveWord)
    comparativeWords.flatMap(a => addComparativeDeclensions(a, SingularGenitiveWord.lemma))

  def addComparativeDeclensions(word: String, lemma: String)(using rules: Seq[DeclensionRule]): Seq[ResultWord] =
    val word_ = Word(word, 16, Option(Gradation("mp", "mm")))
    val declensions = DeclensionUtils.generateDeclensions(word_)
    declensions.map(declension => declension.copy(
      lemma = lemma,
      morphemes = ComparationMorphemes(Comparative, declension.morphemes.asInstanceOf[NomineMorphemes])
    ))

  def generateComparativeWord(SingularGenitiveWord: ResultWord): Seq[String] =
    import SingularGenitiveWord._
    if comparativeHandleExceptionWords(lemma).nonEmpty then comparativeHandleExceptionWords(lemma)
    else formatForComparative(word.toString)

  def formatForComparative(word: String): Seq[String] =
    if(has_e_asBodyVowel(word))
    then Seq(word.dropRight(2) + "empi")
    else Seq(word.dropRight(1) + "mpi")

  def comparativeHandleExceptionWords(lemma: String): Seq[String] =
    lemma match {
      case "kiva" => Seq("kivempi", "kivampi")
      case "liila" => Seq("liilempi", "liilampi")
      case "metka" => Seq("metkempi", "metkampi")
      case "risa" => Seq("risempi", "risampi")
      case _ => Nil
    }

  inline def has_e_asBodyVowel(word: String) =
    Hyphenation.apply(word).size == 2 && word.endsWith("a")

  //===================================================


  //SUPERLATIVE

  def generateSuperlativeForms(SingularGenitiveWord: ResultWord)(using rules: Seq[DeclensionRule]): Seq[ResultWord] =
    val superlativeWords = generateSuperlativeWord(SingularGenitiveWord)
    superlativeWords.flatMap(a => addSuperlativeDeclensions(a, SingularGenitiveWord.lemma))

  def addSuperlativeDeclensions(word: String, lemma: String)(using rules: Seq[DeclensionRule]): Seq[ResultWord] =
    val word_ = Word(word, 36, None)
    val declensions = DeclensionUtils.generateDeclensions(word_)
    declensions.map(declension => declension.copy(
      lemma = lemma,
      morphemes = ComparationMorphemes(Comparative, declension.morphemes.asInstanceOf[NomineMorphemes])
    ))

  def generateSuperlativeWord(SingularGenitiveWord: ResultWord): Seq[String] =
    import SingularGenitiveWord._
    if superlativeHandleExceptionWords(lemma).nonEmpty
    then superlativeHandleExceptionWords(lemma)
    else formatForSuperlative(word.toString)

  def formatForSuperlative(word: String): Seq[String] =
    val last2 = word.takeRight(2)
    (last2(0), last2(1)) match {
      case (a, 'e') if Letters.isConsonant(a)       => Seq(word.dropRight(1) + "in")
      case (a, 'a') if Letters.isConsonant(a)       => Seq(word.dropRight(1) + "in", word.dropRight(1) + "oin")
      case (a, 'ä') if Letters.isConsonant(a)       => Seq(word.dropRight(1) + "in", word.dropRight(1) + "öin")
      case (a, b) if Letters.isVowel(a) && a == b   => Seq(word.dropRight(1) + "in")
      case ('i', 'i')                               => Seq(word.dropRight(2) + "ein")
      case (a, 'i') if a != 'i'                     => Seq(word.dropRight(1) + "ein")
      case _                                        => Seq(word + "in")
    }

  def superlativeHandleExceptionWords(lemma: String): Seq[String] =
    lemma match {
      case "uusi" =>  Seq("uusin")
      case "tosi" =>  Seq("tosin")
      case "täysi" => Seq("täysin")
      case _ => Nil
    }
}


