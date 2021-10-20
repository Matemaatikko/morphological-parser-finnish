package morph_fin.inflection

import morph_fin.kotus_format.{LoadUpdatedKotus, UpdatedWord}
import morph_fin.rulings.GradationHandler
import morph_fin.rulings.morpheme.Morphemes
import morph_fin.rulings.nouns.{AllDeclensionUtils, InflectedWord, Word}
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNomineRules}
import morph_fin.utils.LongestStartingSubstring


case class Ending(ending: String, morphemes: Morphemes)
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
    .filter(_.inflection.rule <= 1)
    .map(a => getWord(a.word, a.inflection.rule, a.inflection.gradationLetter))

  private inline def getWord(lemma: String, ruleNumber: Int, gradationLetterOpt: Option[Char]): Word =
    val gradationOpt = gradationLetterOpt.map(GradationHandler.getGradationByLetter(_))
    Word(lemma, ruleNumber, gradationOpt)

  def register(word: Word) =
    val declensions = AllDeclensionUtils.generateAllDeclections(word)
    val (root, endings) = parse(declensions)
    val index = endingList.indexOf(endings)
    if index == -1 then
      endingList = endingList :+ endings
      rootMap = rootMap :+ (root -> (endingList.size - 1) )
    else
      rootMap = rootMap :+ (root -> index)

  def registerAll =
    words.foreach(register(_))


}
