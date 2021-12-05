package morph_fin.kotus_format

import morph_fin.kotus_format
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Nominative, Plural}
import morph_fin.rulings.nouns.{DeclensionUtils, StructuredWord, Word}
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNounRules}
import morph_fin.rulings.verbs.ConjugationUtils
import morph_fin.utils.{FilesLocation, Hyphenation, Letters}

import java.io.{FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import scala.io.{Codec, Source}



case class SubwordWithInflection(subword: String, inflection: Inflection)

enum ErrorSource:
  case CompoundNotFound
  case CompoundException
  case PluralCompoundNotFound
  case PluralCompoundException

enum UpdatedWord(val value: String):
  //case Compound(word: String, prefix: String, suffix: SubwordWithInflection) extends UpdatedWord(word) //50
  case Compound2(word: String, prefix: SubwordWithInflection, suffix: SubwordWithInflection) extends UpdatedWord(word) // 51
  case StandardBending(word: String, inflection: Inflection) extends UpdatedWord(word)
  case Suffix(word: String, bending: Inflection) extends UpdatedWord(word)
  case Pronoun(word: String) extends UpdatedWord(word)
  case Prefix(word: String) extends UpdatedWord(word)
  case NoInflection(word: String) extends UpdatedWord(word)
  case SuffixError(word: String) extends UpdatedWord(word)
  case Error(word: String, errorSource: ErrorSource, bendingOpt: Option[Inflection] = None) extends UpdatedWord(word)

object PrintUpdatedWord{
  import UpdatedWord._

  /***
   * U - pronoun
   * P - Prefix
   * N - No inflections
   * e - suffix error
   * E - other error
   * W - word
   * S - suffix
   * 1 - Counpound1
   * 2 - Counpound2
   */
  def apply(updated: UpdatedWord) : String = updated match {
    case Pronoun(value)          => s"U:${value}"
    case Prefix(value)           => s"P:${value}"
    case NoInflection(value)        => s"N:${value}"
    case SuffixError(value)      => s"e:${value}"
    case Error(value, source, bendingOpt) => s"E:${value}"+ bendingOpt.map(bendingString(_)).getOrElse("")
    case StandardBending(word, bending) =>
      s"W:${word}" + bendingString(bending)
    case Suffix(word, bending) =>
      s"S:${word}" + bendingString(bending)
//    case Compound(word, prefix, suffix) =>
//      s"1:${word}:P:${prefix}:S:${suffix.subword}" + bendingString(suffix.inflection)
    case Compound2(word, prefix, suffix) =>
      s"2:${word}:P:${toString(prefix)}:S:${toString(suffix)}"
  }
  def toString(bendingWord: SubwordWithInflection): String =
    bendingWord.subword + bendingString(bendingWord.inflection)

  def bendingString(bending: Inflection): String = s":B:${bending.rule}" + bending.gradationLetter.map(l => s":L:$l").getOrElse("")
}

val numerals = Seq("yksi", "kaksi", "kolme", "neljä", "viisi", "kuusi", "seitsemän", "kahdeksan", "yhdeksän")
val orderNumerals = Seq("yhdes", "kahdes", "kolmas", "neljäs", "viides", "kuudes", "seitsemäs", "kahdeksas", "yhdeksäs")

val remove = Seq(
  "ikenet",
  "nuoripari",

  "korkeakoulu", "jalokivi", "viileäkaappi", "raaka-aine",
  "missä", "mikin", "missäkin",

  "hapan",
  "puolisataa"
)
++ numerals.map(_ + "toista")
++ numerals.drop(1).map(_ + "kymmentä")
++ numerals.drop(1).map(_ + "sataa")
++ numerals.drop(1).map(_ + "tuhatta")
++ orderNumerals.map(_ + "toista")
++ orderNumerals.drop(1).map(_ + "kymmenes")
++ orderNumerals.drop(1).map(_ + "sadas")
++ orderNumerals.drop(1).map(_ + "tuhannes")

val additions = Seq(
  Entry(KotusWord.Word("raamattu"), Some(Inflection(4, Some('C')))),
  Entry(KotusWord.Word("torvisieni"), Some(Inflection(26, None))),
  Entry(KotusWord.Word("viinimarja"), Some(Inflection(9, None))),
  Entry(KotusWord.Suffix("pestävä"), Some(Inflection(10, None))),
  Entry(KotusWord.Word("kulmain"), Some(Inflection(33, None))),
  Entry(KotusWord.Word("särkynyt"), Some(Inflection(47, None))),

  Entry(KotusWord.Word("korkeakoulu"), Some(Inflection(1, None))),
  Entry(KotusWord.Word("jalokivi"), Some(Inflection(7, None))),
  Entry(KotusWord.Word("viileäkaappi"), Some(Inflection(5, Some('B')))),
  Entry(KotusWord.Word("raaka-aine"), Some(Inflection(48, None))),
  //No inflection pronouns
  Entry(KotusWord.Word("missä"), Some(Inflection(99, None))),
  Entry(KotusWord.Word("mikin"), Some(Inflection(99, None))),
  Entry(KotusWord.Word("missäkin"), Some(Inflection(99, None))),

  Entry(KotusWord.Word("hapan"), Some(Inflection(34, Some('B')))),
  Entry(KotusWord.Word("puolisata"), Some(Inflection(51, None))),
  Entry(KotusWord.Word("toista"), Some(Inflection(99, None))),
)
++ numerals.map(_ + "toista").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))
++ numerals.drop(1).map(_ + "kymmen").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))
++ numerals.drop(1).map(_ + "sata").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))
++ numerals.drop(1).map(_ + "tuhat").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))
++ orderNumerals.map(_ + "toista").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))
++ orderNumerals.drop(1).map(_ + "kymmenes").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))
++ orderNumerals.drop(1).map(_ + "sadas").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))
++ orderNumerals.drop(1).map(_ + "tuhannes").map(value => Entry(KotusWord.Word(value), Some(Inflection(51, None))))

val corrections = Seq(
  UpdatedWord.Compound2("nuoripari", SubwordWithInflection("nuori", Inflection(26, None)), SubwordWithInflection("pari", Inflection(5, None)))
)

case class Data(
                 list: Seq[Entry],
                 words: Map[String, Seq[Entry]],
                 suffixes: Seq[Entry],
                 pluralSuffixes: Seq[(String, Entry)],
                 pluralWords: Map[String, Seq[Entry]]
               )

object ReformatKotus {

  given Seq[DeclensionRule] = LoadAndParseNounRules.rules


  def fixIlmeinen(entry: Entry): Entry = entry match {
    case Entry(word, Some(bending)) if word.value.endsWith("ilmeinen") && bending.rule == 18 => Entry(word, Some(bending.copy(rule = 38)))
    case _ => entry
  }

  def apply(rulings: Seq[DeclensionRule]): Seq[UpdatedWord] =
    val list: Seq[Entry] = ParseKotus()
      .filterNot(a => remove.contains(a.word.value))
      .map(fixIlmeinen(_))
      ++ additions

    //===================================

    val validSuffixes: Seq[Entry] = list
      .filter(entry => entry.inflectionOpt.nonEmpty &&
        entry.word.noPrefix)

    val pluralSuffixes: Seq[(String, Entry)]  = validSuffixes.filter(_.inflectionOpt.exists(_.rule < 50)).flatMap(entry =>
        DeclensionUtils.generateDeclensions(EntryToWord(entry).get)
          .find(elem => elem.morphemes.is(Nominative, Plural))
          .map(casing => casing.word.toString -> entry)
    )

    //===================================

    val pluralWords: Map[String, Seq[Entry]]  = list.filter(_.inflectionOpt.exists(_.rule < 50)).flatMap(entry =>
        DeclensionUtils.generateDeclensions(EntryToWord(entry).get)
          .find(elem => elem.morphemes.is(Nominative, Plural))
          .map(casing => casing.word.toString -> entry)
    ).groupBy(_._1).map(a => a._1 -> a._2.map(_._2))


    val listMap: Map[String, Seq[Entry]]  = list.map(entry => entry.word.value -> entry).groupBy(_._1).map(a => a._1 -> a._2.map(_._2))

    //===================================


    val data = Data(list, listMap, validSuffixes, pluralSuffixes, pluralWords)

    def numeralFixing(word: UpdatedWord) = word match {
      case UpdatedWord.Compound2(s"${start}kymmen", prefix, suffix)  => UpdatedWord.Compound2(s"${start}kymmentä", prefix, suffix)
      case UpdatedWord.Compound2(s"${start}sata", prefix, suffix)  => UpdatedWord.Compound2(s"${start}sataa", prefix, suffix)
      case a => a
    }

    (list.map(resolveValue(_)(data)) ++ corrections).filter(_.value != "toista").map(numeralFixing(_))
  end apply



  inline def resolveValue(entry: Entry)(data: Data): UpdatedWord =
    import KotusWord._
    entry match {
      case Entry(Prefix(value), _)             => UpdatedWord.Prefix(value)
      case Entry(Suffix(value), Some(bending)) => UpdatedWord.Suffix(value, bending)
      case Entry(Suffix(value), None)          => UpdatedWord.SuffixError(value)
      case Entry(KotusWord.Word(value), option) if option.isEmpty || option.exists(_.rule == 50)  => resolveCompound(value, false, option)(data)
      case Entry(KotusWord.Word(value), Some(bending)) if bending.rule == 51 => resolveCompound(value, true, Some(bending))(data)
      case Entry(KotusWord.Word(value), Some(bending)) if bending.rule == 101 => UpdatedWord.Pronoun(value)
      case Entry(KotusWord.Word(value), Some(bending)) if bending.rule == 99 => UpdatedWord.NoInflection(value)
      case Entry(KotusWord.Word(value), Some(bending)) => UpdatedWord.StandardBending(value, bending)
    }


  /**
   * Resolve singular compound word
   * ==============================
   * Examples: normaalijakauma, raaka-aine
   */
  inline def resolveCompound(word: String, bendBoth: Boolean, bendingOpt: Option[Inflection])
                     (data: Data): UpdatedWord =
    try
      val resultOpt = findCompound(word)(data)
      resultOpt match {
        case Some((prefix, suffixEntry)) =>
          generateCompound(word, bendBoth, prefix, suffixEntry)(data)
        case None =>
          if(word.last == 't') resolveCompoundPlural(word, bendBoth, bendingOpt)(data)
          else UpdatedWord.Error(word, ErrorSource.CompoundNotFound, bendingOpt)
      }
    catch
      case e: Exception =>
        //println(e.getMessage() + s"Word: ${value}")
        if(word.last == 't') resolveCompoundPlural(word, bendBoth, bendingOpt)(data)
        else UpdatedWord.Error(word, ErrorSource.CompoundException, bendingOpt)

  /***
   * Tries to split compound word by finding suitable suffix for the word.
   * Example: raaka-aine -> (String: raaka, Entry: aine)
   */
  inline def findCompound(word: String)(data: Data): Option[(String, Entry)] =
    val suffixOpt: Option[Entry] =
      data.suffixes
        .filter(entry => word != entry.word.value && word.endsWith(entry.word.value))
        .sortWith((a, b) => compareEntry(a, b))
        .headOption
    suffixOpt.map(suffix => {
      val prefix = word.dropRight(suffix.word.value.length)
      val updatedPrefix = if prefix.last == '-' then prefix.dropRight(1) else prefix
      (updatedPrefix, suffix)
    })

  //a > b
  inline def compareEntry(a: Entry, b: Entry): Boolean =
    (a, b) match {
      case (Entry(word, Some(bending)), Entry(word1, None)) => true
      case (Entry(word, None), Entry(word1, Some(bending1))) => false
      case (Entry(word, Some(bending)), Entry(word1, Some(bending1))) => word.value.length > word1.value.length
      case (Entry(word, None), Entry(word1, None)) => word.value.length > word1.value.length
    }

  inline def findEntry(word: String)(data: Data): Option[Entry] =
    data.words(word)
      .sortWith((a, b) => compareEntry(a, b))
      .headOption


  inline def generateCompound(word: String, bendBoth: Boolean, prefix: String, suffixEntry: Entry)(data: Data): UpdatedWord =
    val suffixWord = SubwordWithInflection(suffixEntry.word.value, suffixEntry.inflectionOpt.get)
    if !bendBoth && suffixWord.inflection.rule != 99 then
      UpdatedWord.StandardBending(prefix + suffixEntry.word.value, suffixEntry.inflectionOpt.get)
    else if !bendBoth && suffixWord.inflection.rule == 99 then
      UpdatedWord.NoInflection(word)
    else
      val prefixEntry = findEntry(prefix)(data).get
      val prefixWord = SubwordWithInflection(prefixEntry.word.value, prefixEntry.inflectionOpt.get)
      UpdatedWord.Compound2(word, prefixWord, suffixWord)


  /**
   * Resolve plural compound word
   * ============================
   * Examples: materiaali-toiminnot, isot-aivot
   *
   */
  inline def resolveCompoundPlural(pluralWord: String, bendBoth: Boolean, bendingOpt: Option[Inflection])
                           (data: Data): UpdatedWord =
    try
      val resultOpt = findCompoundPlural(pluralWord)(data)
      resultOpt match {
        case Some((prefix, suffixEntry)) =>
          generateCompoundPlural(pluralWord, bendBoth, prefix, suffixEntry)(data)
        case None =>
          UpdatedWord.Error(pluralWord, ErrorSource.PluralCompoundNotFound, bendingOpt)
      }
    catch
      case e: Exception =>
        e.printStackTrace
        println(s"Word: ${pluralWord}")
        UpdatedWord.Error(pluralWord, ErrorSource.PluralCompoundException, bendingOpt)

  inline def findCompoundPlural(word: String)(data: Data): Option[(String, Entry)] =
    val tupleOpt: Option[(String, Entry)] = data.pluralSuffixes
      .filter(tuple => word.endsWith(tuple._1) && word != tuple._1)
      .sortWith((a, b) => compareEntry(a._2, b._2))
      .headOption
    tupleOpt.map(tuple => {
      val prefix = word.dropRight(tuple._1.length)
      val updatedPrefix = if prefix.last == '-' then prefix.dropRight(1) else prefix
      (updatedPrefix, tuple._2)
    })

  inline def findPluralEntry(word: String)(data: Data): Option[Entry] =
    data.pluralWords(word)
      .sortWith((a, b) => compareEntry(a, b))
      .headOption

  inline def generateCompoundPlural(pluralWord: String, bendBoth: Boolean, prefix: String, suffixEntry: Entry)(data: Data): UpdatedWord =
    val suffixWord = SubwordWithInflection(suffixEntry.word.value, suffixEntry.inflectionOpt.get)
    if !bendBoth && suffixWord.inflection.rule != 99 then
      UpdatedWord.StandardBending(prefix + suffixEntry.word.value, suffixEntry.inflectionOpt.get)
    else if !bendBoth && suffixWord.inflection.rule == 99 then
      UpdatedWord.NoInflection(pluralWord)
    else
      val prefixEntry = findPluralEntry(prefix)(data).get
      val prefixWord = SubwordWithInflection(prefixEntry.word.value, prefixEntry.inflectionOpt.get)
      UpdatedWord.Compound2(pluralWord, prefixWord, suffixWord)


  val updatedFilename = FilesLocation.files_path + "/result/kotus_updated.txt"


  def reformat: Unit =
    val nomineBendings = LoadAndParseNounRules.rules
    val result: Seq[UpdatedWord] = apply(nomineBendings)
/*    val errors = result.flatMap(word => word match {
      case a: UpdatedWord.Error => Some(a)
      case _                    => None
    })*/
    val writer = new OutputStreamWriter(new FileOutputStream(updatedFilename), StandardCharsets.UTF_8)
    result.foreach(value => writer.write(PrintUpdatedWord(value) + "\n"))
    writer.close()

}
