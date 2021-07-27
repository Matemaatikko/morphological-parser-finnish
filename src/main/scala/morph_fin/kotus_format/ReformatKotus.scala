package morph_fin.kotus_format

import morph_fin.kotus_format
import morph_fin.rulings._

import scala.io.{Codec, Source}


val fileName = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish\\src\\main\\files\\kotus-sanalista_v1_original.xml"
val updatedFileName = "C:\\Users\\juho_.DESKTOP-UEAL9SV\\IdeaProjects\\morphological-parser-finnish\\src\\main\\files\\kotus_updated.txt"

case class BendingWord(value: String, bending: Bending)

enum UpdatedWord:
  case Compound(word: String, prefix: String, suffix: BendingWord) //50
  case Compound2(word: String, prefix: BendingWord, suffix: BendingWord) // 51
  case StandardBending(word: String, bending: Bending)
  case Suffix(word: String, bending: Bending)
  case Pronoun(word: String)
  case Prefix(word: String)
  case NoBending(word: String)
  case Adverb(word: String)
  case AdverbSuffix(word: String)
  case Error(word: String)
  case CompoundError(word: String, bending: Bending)

object PrintUpdatedWord{
  import UpdatedWord._
  def apply(updated: UpdatedWord) : String = updated match {
    case Pronoun(value)          => s"U:${value}"
    case Prefix(value)           => s"P:${value}"
    case NoBending(value)        => s"U:${value}"
    case Adverb(value)           => s"U:${value}"
    case AdverbSuffix(value)     => s"U:${value}"
    case Error(value)            => s"U:${value}"
    case CompoundError(value, _) => s"E:${value}"
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


val customUpdates = Map(
  "housut" -> ("housu", 1)
)
val remove = Seq("ikenet")

val additions = Seq(
  Entry(KotusWord.Word("raamattu"), Some(Bending(4, Some('C'))))
)

object ReformatKotus {

  def apply(rulings: Seq[ExtensiveRuling]): Seq[UpdatedWord] =
    val list: Seq[Entry] = (
      for(line: String <- Source.fromFile(fileName)(Codec.UTF8).getLines)
      yield
        if(line.startsWith("<st>"))
          val parsedLine = ParseLine(line)
          if(parsedLine.word.value == "housut") Some(Entry(KotusWord.Word("housu"), Some(Bending(1, None))))
          else Some(parsedLine)
        else None
      ).flatten.toSeq.filterNot(a => remove.contains(a.word.value)) ++ additions
    import KotusWord._
    val validSuffixes = list.filter(entry => entry.bending.nonEmpty && entry.bending.exists(bend => bend.rule != 99 && bend.rule != 101) && entry.word.noPrefix)
    val pluralSuffixes = list.filter(_.bending.exists(_.rule < 50)).map(entry =>
      entry ->
        GenerateCases.apply(rulings, GenerateCases.getWord(entry))
          .find(elem => elem.tpe == Type.Plural && elem.cse == Case.Nominative)
          .map(_.word)
    ).flatMap(tuple => tuple._2.map(a => tuple._1 -> a))

    def resolveValue(entry: Entry): UpdatedWord =
      entry match {
        case Entry(Prefix(value), _)             => UpdatedWord.Prefix(value)
        case Entry(Suffix(value), Some(bending)) => UpdatedWord.Suffix(value, bending)
        case Entry(Suffix(value), None) =>
          if(value.endsWith("sti")) UpdatedWord.AdverbSuffix(value)
          else UpdatedWord.Error(value)
        case Entry(Word(value), None) if value.endsWith("sti") => UpdatedWord.Adverb(value)
        case Entry(Word(value), Some(bending)) if bending.rule == 101 => UpdatedWord.Pronoun(value)
        case Entry(Word(value), Some(bending)) if bending.rule == 99 => UpdatedWord.NoBending(value)
        case Entry(Word(value), option) if option.isEmpty || option.exists(_.rule == 50)  => resolveCompound(value, false, option)
        case Entry(Word(value), Some(bending)) if bending.rule == 51 => resolveCompound(value, true, Some(bending))
        case Entry(Word(value), Some(bending)) => UpdatedWord.StandardBending(value, bending)
      }

    def resolveCompound(value: String, bendBoth: Boolean, bendingOpt: Option[Bending]): UpdatedWord =
      try
        val resultOpt = findCompound(value)
        resultOpt match {
          case Some((prefix, suffixEntry)) =>
            val suffixWord = BendingWord(suffixEntry.word.value, suffixEntry.bending.getOrElse(throw new Exception(s"No bending for suffix: ${suffixEntry.word.value}, word: ${value}")))
            if !bendBoth then UpdatedWord.Compound(value, prefix, suffixWord)
            else
              val prefixEntry = findEntry(prefix).getOrElse(throw new Exception(s"No such prefix: $prefix, word: ${value}"))
              UpdatedWord.Compound2(
                value,
                BendingWord(prefixEntry.word.value, prefixEntry.bending.getOrElse(throw new Exception(s"No bending for prefix: ${prefixEntry.word.value}, word: ${value}"))),
                BendingWord(suffixEntry.word.value, suffixEntry.bending.getOrElse(throw new Exception(s"No bending for suffix: ${suffixEntry.word.value}, word: ${value}"))),
              )
          case None =>
            resolveCompoundPlural(value, bendBoth, bendingOpt)

        }
      catch
        case e: Exception =>
          println(e.getMessage())
          resolveCompoundPlural(value, bendBoth, bendingOpt)

    def findCompound(word: String): Option[(String, Entry)] =
      val entryOpt: Option[Entry] =
        validSuffixes
          .filter(entry => word != entry.word.value && word.endsWith(entry.word.value))
          .sortWith((a, b) => a.word.value.length > b.word.value.length)
          .headOption
      entryOpt.map(entry => {
        val prefix = word.dropRight(entry.word.value.length)
        val updatedPrefix = if prefix.last == '-' then prefix.dropRight(1) else prefix
        (updatedPrefix, entry)
      })

    def findEntry(word: String): Option[Entry] =
      list.find(entry => entry.word.value == word)

    def resolveCompoundPlural(pluralValue: String, bendBoth: Boolean, bendingOpt: Option[Bending]): UpdatedWord =
      val resultOpt = findCompoundPlural(pluralValue)
      resultOpt match {
        case Some((prefix, suffixEntry)) =>
          val suffixWord = BendingWord(suffixEntry.word.value, suffixEntry.bending.getOrElse(throw new Exception(s"No bending for suffix: ${suffixEntry.word.value}, word: ${pluralValue}")))
          if !bendBoth then UpdatedWord.Compound(prefix + suffixWord.value, prefix, suffixWord)
          else
            val prefixEntry = findEntryPlural(prefix).getOrElse(throw new Exception(s"No such plural prefix: $prefix, word: ${pluralValue}"))
            UpdatedWord.Compound2(
              prefixEntry.word.value + suffixEntry.word.value,
              BendingWord(prefixEntry.word.value, prefixEntry.bending.getOrElse(throw new Exception(s"No bending for prefix: ${prefixEntry.word.value}, word: ${pluralValue}"))),
              BendingWord(suffixEntry.word.value, suffixEntry.bending.getOrElse(throw new Exception(s"No bending for suffix: ${suffixEntry.word.value}, word: ${pluralValue}"))),
            )
        case None =>
          bendingOpt.map(UpdatedWord.CompoundError(pluralValue, _)).getOrElse(UpdatedWord.Error(pluralValue))
      }

    def findCompoundPlural(word: String): Option[(String, Entry)] =
      val tupleOpt: Option[(Entry, String)] = pluralSuffixes.find(tuple => word.endsWith(tuple._2) && word != tuple._2)
      tupleOpt.map(tuple => {
        val prefix = word.dropRight(tuple._2.length)
        val updatedPrefix = if prefix.last == '-' then prefix.dropRight(1) else prefix
        (updatedPrefix, tuple._1)
      })

    def findEntryPlural(word: String): Option[Entry] =
      pluralSuffixes.find(tuple => tuple._2 == word).map(_._1)

    list.map(resolveValue(_))
  end apply

  def save(list: Seq[UpdatedWord]): Unit =
    import java.io._
    import java.nio.charset.StandardCharsets
    //val str: String = list.map(PrintUpdatedWord(_)).mkString("\n")
    val writer = new OutputStreamWriter(new FileOutputStream(updatedFileName), StandardCharsets.UTF_8)
    list.foreach(value => writer.write(PrintUpdatedWord(value) + "\n"))
    writer.close()
}
