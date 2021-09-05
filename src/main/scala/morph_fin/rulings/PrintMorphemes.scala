package morph_fin.rulings

import morph_fin.rulings.PossessiveSuffix.*



object FilePrint {
  def apply(morphemes: Morphemes): String = "TODO"

  /*morphemes match {
    case NomineMorphemes(cse, form) => printGrammaticalNumber(form) + ":" + printCase(cse)
    case MorphemesWithPosSuffix(morphemes_, suffix) => apply(morphemes_) + printSuffix(Some(suffix))
    case Standard(modus, tempus, persona, mode) =>
      printModus(modus) + ":" + printTempus(tempus) + ":" + printPersona(persona) + printMode(mode)
    case AInfinitive(false) =>
      "Inf1"
    case AInfinitive(true) =>
      "Inf1:L"
    case EInfinitive(cse, voice) =>
      "Inf2:" + printCase(cse) + ":" + printVoice(voice)
    case MAInfinitive(cse, voice) =>
      "Inf3:" + printCase(cse) + ":" + printVoice(voice)
    case InfinitiveIV => "Inf4:"
    case InfinitiveV => "Inf5:"
    case InfinitiveVI => "Inf6:"
    case InfinitiveVII => "Inf7:"
    case vAParticiple(Active) => "Par1:"
    case vAParticiple(Passive) => "Par2:"
    case nUtParticiple => "Par3:"
    case tUParticiple => "Par4:"
    case AgentParticiple => "Par5:"
    case NegativeParticiple => "Par6:"
  }

  def printSuffix(suffixOpt: Option[PossessiveSuffix]): String =
    suffixOpt match {
      case Some(Body) => ":Pos:Body"
      case Some(SingularFirst) => ":Pos:S1"
      case Some(SingularSecond) => ":Pos:S2"
      case Some(PluralFirst) => ":Pos:P1"
      case Some(PluralSecond) => ":Pos:P2"
      case Some(ThirdPos) => ":Pos:3"
      case None         => ""
    }

  def printDeclension(declensionOpt: Option[NomineMorphemes]): String =
    declensionOpt match {
      case Some(NomineMorphemes(cse, number)) => ":"  + printGrammaticalNumber(number) + ":" + printCase(cse)
      case None             => ""
    }

  def printCase(cse: Case): String =
    cse match {
      case Nominative  => "Nom"
      case Genitive    => "Gen"
      case Accusative  => "Akk"
      case Partitive   => "Par"
      case Inessive    => "Ine"
      case Elative     => "Ela"
      case Illative    => "Ill"
      case Adessive    => "Ade"
      case Ablative    => "Abl"
      case Allative    => "All"
      case Essive      => "Ess"
      case Translative => "Tra"
      case Instructive => "Ins"
      case Abessive    => "Abe"
      case Comitative  => "Kom"
    }

  def printGrammaticalNumber(number: GrammaticalNumber): String =
    number match {
      case Singular => "S"
      case Plural   => "P"
    }

  def printModus(modus: Modus): String =
    modus match {
      case Indicative  => "Ind"
      case Conditional => "Con"
      case Potential   => "Pot"
      case Imperative  => "Imp"
    }

  def printTempus(tempus: Tempus): String =
    tempus match {
      case Present     => "Pre"
      case Imperfect   => "Imp"
      case Perfect     => "Per"
      case Pluperfect  => "Plp"
    }

  def printPersona(persona: Persona): String =
    import Persona._
    persona match {
      case Passive              => "Pas"
      case Active(form, number) => "Akt:" + printGrammaticalNumber(form) + printPerson(number)
    }

  def printPerson(person: Person): String =
    person match {
      case First  => "1"
      case Second => "2"
      case Third  => "3"
    }

  def printMode(mode: Mode): String =
    mode match {
      case Positive  => ""
      case Negative  => ":Neg"
    }

  def printVoice(voice: Voice): String =
    voice match {
      case Active    => "Akt"
      case Passive   => "Pas"
    }*/
}