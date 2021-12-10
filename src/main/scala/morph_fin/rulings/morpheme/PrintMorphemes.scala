package morph_fin.rulings.morpheme

import com.sun.istack.internal.Nullable
import morph_fin.rulings.morpheme.PossessiveSuffix
import morph_fin.rulings.morpheme.{AInfinitive, AInfinitiveLong, Abessive, Ablative, Accusative, Active, Adessive, AgentParticiple, Allative, Append, Comitative, Comparative, Conditional, EInfinitive, Elative, Essive, Finite, General, Genitive, Illative, Imperative, Imperfect, Indicative, Inessive, InfinitiveIV, InfinitiveV, InfinitiveVI, InfinitiveVII, Instructive, MAInfinitive, Morpheme, Morphemes, Negative, NegativeParticiple, Nominative, Noun, Partitive, Passive, Perfect, Pluperfect, Plural, PluralThird, Positive, Potential, Present, Singular, SingularThird, Superlative, Translative, stiAdverb}



object PrintMorphemes {

  given Conversion[String, Option[String]] with
    def apply(str: String): Option[String] = Some(str)

  val morphemeMap: Map[Morpheme, Option[String]] = Map(
    Nominative  -> "Nom",
    Genitive    -> "Gen",
    Accusative  -> "Acc",
    Partitive   -> "Par",
    Inessive    -> "Ine",
    Elative     -> "Ela",
    Illative    -> "Ill",
    Adessive    -> "Ade",
    Ablative    -> "Abl",
    Allative    -> "All",
    Essive      -> "Ess",
    Translative -> "Tra",
    Instructive -> "Ins",
    Abessive    -> "Abe",
    Comitative  -> "Com",

    Singular -> "S",
    Plural   -> "P",

    Positive -> None,
    Negative -> "Neg",

    Active -> "Act",
    Passive -> "Pass",

    Indicative  -> "Ind",
    Conditional -> "Con",
    Potential   -> "Pot",
    Imperative  -> "Impera",

    Present    -> "Pre",
    Perfect    -> "Per",
    Imperfect  -> "Imp",
    Pluperfect -> "Plp",

    General -> "Gen",

    SingularFirst  -> "S1",
    SingularSecond -> "S2",
    SingularThird  -> "S3",

    PluralFirst  -> "P1",
    PluralSecond -> "P2",
    PluralThird  -> "P3",

    Comparative -> "Comp",
    Superlative -> "Sup",

    PossessiveSuffix.Body           -> "Poss-Body",
    PossessiveSuffix.SingularFirst  -> "Poss-S1",
    PossessiveSuffix.SingularSecond -> "Poss-S2",
    PossessiveSuffix.PluralFirst    -> "Poss-P1",
    PossessiveSuffix.PluralSecond   -> "Poss-P2",
    PossessiveSuffix.ThirdPos       -> "Poss-3",

    stiAdverb -> "Adv"
  )


  def print(morpheme: Morpheme): Option[String] = morphemeMap.get(morpheme).flatten

  def apply(morphemes: Morphemes): String = toList(morphemes).mkString(":")

  def toList(morphemes: Morphemes): Seq[String] =
    morphemes match {
    case Append(morpheme, rest) =>
      val prt = print(morpheme)
      prt match {
        case Some(value) =>
          toList(rest) :+ value
        case None => toList(rest)
      }
    case Noun                   => Seq("Noun")
    case Finite                 => Seq("Fin")
    case AInfinitive            => Seq("AInf")
    case AInfinitiveLong        => Seq("AInfL")
    case EInfinitive            => Seq("EInf")
    case MAInfinitive           => Seq("MaInf")
    case InfinitiveIV           => Seq("Inf4")
    case InfinitiveV            => Seq("Inf5")
    case InfinitiveVI           => Seq("Inf6")
    case InfinitiveVII          => Seq("Inf7")
    case vAParticiple           => Seq("vAPar")
    case nUtParticiple          => Seq("nUtPar")
    case tUParticiple           => Seq("tUPar")
    case AgentParticiple        => Seq("agePar")
    case NegativeParticiple     => Seq("negPar")
  }

}