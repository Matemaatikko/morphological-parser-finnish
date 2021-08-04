package morph_fin.kotus_format

import morph_fin.kotus_format
import morph_fin.rulings.*
import morph_fin.rulings.nomines.{GenerateNomineBendings, Gradation, LoadAndParseNomineRules, NomineBending, Word}
import morph_fin.utils.Hyphenation

import scala.io.{Codec, Source}


val fileName = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish\\src\\main\\files\\kotus-sanalista_v1_original.xml"
val updatedFileName = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish\\src\\main\\files\\kotus_updated.txt"

case class BendingWord(value: String, bending: Bending)

enum ErrorSource:
  case CompoundNotFound
  case CompoundException
  case PluralCompoundNotFound
  case PluralCompoundException

enum UpdatedWord:
  case Compound(word: String, prefix: String, suffix: BendingWord) //50
  case Compound2(word: String, prefix: BendingWord, suffix: BendingWord) // 51
  case StandardBending(word: String, bending: Bending)
  case Suffix(word: String, bending: Bending)
  case Pronoun(word: String)
  case Prefix(word: String)
  case NoBending(word: String)
  case SuffixError(word: String)
  case Error(word: String, errorSource: ErrorSource, bendingOpt: Option[Bending] = None)

object PrintUpdatedWord{
  import UpdatedWord._
  def apply(updated: UpdatedWord) : String = updated match {
    case Pronoun(value)          => s"U:${value}"
    case Prefix(value)           => s"P:${value}"
    case NoBending(value)        => s"N:${value}"
    case SuffixError(value)      => s"E:-${value}"
    case Error(value, source, bendingOpt) => s"E:${value}"+ bendingOpt.map(bendingString(_)).getOrElse("")
    case StandardBending(word, bending) =>
      s"W:${word}" + bendingString(bending)
    case Suffix(word, bending) =>
      s"S:${word}" + bendingString(bending)
    case Compound(word, prefix, suffix) =>
      s"C1:${word}:P:${prefix}:S:${suffix.value}" + bendingString(suffix.bending)
    case Compound2(word, prefix, suffix) =>
      s"C2:${word}:P:${toString(prefix)}:S:${toString(suffix)}"
  }
  def toString(bendingWord: BendingWord): String =
    bendingWord.value + bendingString(bendingWord.bending)

  def bendingString(bending: Bending): String = s":B:${bending.rule}" + bending.gradationLetter.map(l => s":L:$l").getOrElse("")
}


val remove = Seq("ikenet")

val additions = Seq(
  Entry(KotusWord.Word("raamattu"), Some(Bending(4, Some('C')))),
  Entry(KotusWord.Word("torvisieni"), Some(Bending(26, None))),
  Entry(KotusWord.Word("viinimarja"), Some(Bending(9, None))),
  Entry(KotusWord.Suffix("pestävä"), Some(Bending(10, None))),
  Entry(KotusWord.Word("kulmain"), Some(Bending(33, None))),
  Entry(KotusWord.Word("särkynyt"), Some(Bending(47, None)))
)

case class Data(
                 list: Seq[Entry],
                 words: Map[String, Seq[Entry]],
                 suffixes: Seq[Entry],
                 pluralSuffixes: Seq[(String, Entry)],
                 pluralWords: Map[String, Seq[Entry]]
               )

object ReformatKotus {

  import KotusWord._

  def apply(rulings: Seq[NomineBending]): Seq[UpdatedWord] =
    val list: Seq[Entry] = (
      for(line: String <- Source.fromFile(fileName)(Codec.UTF8).getLines)
      yield
        if(line.startsWith("<st>")) Some(ParseLine(line))
        else None
      ).flatten.toSeq.filterNot(a => remove.contains(a.word.value)) ++ additions

    //===================================

    val validSuffixes: Seq[Entry] = list
      .filter(entry => entry.bending.nonEmpty &&
        entry.bending.exists(bend => bend.rule != 99 && bend.rule != 101) &&
        entry.word.noPrefix)

    val pluralSuffixes: Seq[(String, Entry)]  = validSuffixes.filter(_.bending.exists(_.rule < 50)).flatMap(entry =>
        GenerateNomineBendings.apply(rulings, EntryToWord(entry).get)
          .find(elem => elem.morphemes == NomineMorphemes(Case.Nominative, Form.Plural))
          .map(casing => casing.word -> entry)
    )

    //===================================

    val pluralWords: Map[String, Seq[Entry]]  = list.filter(_.bending.exists(_.rule < 50)).flatMap(entry =>
        GenerateNomineBendings.apply(rulings, EntryToWord(entry).get)
          .find(elem => elem.morphemes == NomineMorphemes(Case.Nominative, Form.Plural))
          .map(casing => casing.word -> entry)
    ).groupBy(_._1).map(a => a._1 -> a._2.map(_._2))


    val listMap: Map[String, Seq[Entry]]  = list.map(entry => entry.word.value -> entry).groupBy(_._1).map(a => a._1 -> a._2.map(_._2))

    //===================================


    val data = Data(list, listMap, validSuffixes, pluralSuffixes, pluralWords)

    list.map(resolveValue(_)(data))
  end apply



  inline def resolveValue(entry: Entry)(data: Data): UpdatedWord =
    entry match {
      case Entry(Prefix(value), _)             => UpdatedWord.Prefix(value)
      case Entry(Suffix(value), Some(bending)) => UpdatedWord.Suffix(value, bending)
      case Entry(Suffix(value), None)          => UpdatedWord.SuffixError(value)
      case Entry(KotusWord.Word(value), option) if option.isEmpty || option.exists(_.rule == 50)  => resolveCompound(value, false, option)(data)
      case Entry(KotusWord.Word(value), Some(bending)) if bending.rule == 51 => resolveCompound(value, true, Some(bending))(data)
      case Entry(KotusWord.Word(value), Some(bending)) if bending.rule == 101 => UpdatedWord.Pronoun(value)
      case Entry(KotusWord.Word(value), Some(bending)) if bending.rule == 99 => UpdatedWord.NoBending(value)
      case Entry(KotusWord.Word(value), Some(bending)) => UpdatedWord.StandardBending(value, bending)
    }


  /**
   * Resolve singular compound word
   * ==============================
   * Examples: normaalijakauma, raaka-aine
   */
  inline def resolveCompound(word: String, bendBoth: Boolean, bendingOpt: Option[Bending])
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
    val suffixWord = BendingWord(suffixEntry.word.value, suffixEntry.bending.get)
    if !bendBoth then UpdatedWord.Compound(word, prefix, suffixWord)
    else
      val prefixEntry = findEntry(prefix)(data).get
      val prefixWord = BendingWord(prefixEntry.word.value, prefixEntry.bending.get)
      UpdatedWord.Compound2(word, prefixWord, suffixWord)


  /**
   * Resolve plural compound word
   * ============================
   * Examples: materiaali-toiminnot, isot-aivot
   *
   */
  inline def resolveCompoundPlural(pluralWord: String, bendBoth: Boolean, bendingOpt: Option[Bending])
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
    val suffixWord = BendingWord(suffixEntry.word.value, suffixEntry.bending.get)
    if !bendBoth then UpdatedWord.Compound(pluralWord, prefix, suffixWord)
    else
      val prefixEntry = findPluralEntry(prefix)(data).get
      val prefixWord = BendingWord(prefixEntry.word.value, prefixEntry.bending.get)
      UpdatedWord.Compound2(pluralWord, prefixWord, suffixWord)


  def save(list: Seq[UpdatedWord]): Unit =
    import java.io._
    import java.nio.charset.StandardCharsets
    val writer = new OutputStreamWriter(new FileOutputStream(updatedFileName), StandardCharsets.UTF_8)
    list.foreach(value => writer.write(PrintUpdatedWord(value) + "\n"))
    writer.close()

  def reformat: Unit =
    val nomineBendings = LoadAndParseNomineRules.rules
    val result: Seq[UpdatedWord] = apply(nomineBendings)
/*    val errors = result.flatMap(word => word match {
      case a: UpdatedWord.Error => Some(a)
      case _                    => None
    })*/
    save(result)
}
