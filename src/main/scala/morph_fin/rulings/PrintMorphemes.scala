package morph_fin.rulings

import morph_fin.rulings.PossessiveSuffix._

object Print {
  import VerbMophemes._
  def apply(morphemes: Morphemes): String = morphemes match {
    case NomineMorphemes(cse, form) => cse.toString + " " + form.toString
    case Standard(modus, tempus, Persona.Passive, mode) =>
      modus.toString + " " + tempus.toString + " Passive" + (if mode == Negative then " Negative" else "")
    case Standard(modus, tempus, Persona.Active(form, number), mode) =>
      modus.toString + " " + tempus.toString + " Active " + form.toString + " " + number.toString + (if mode == Negative then " Negative" else "")
    case InfinitiveI(Type.Short) =>
      "Infinitive I"
    case InfinitiveI(Type.Long) =>
      "Infinitive I Long"
    case InfinitiveII(cse, voice) =>
      "Infinitive II " + voice.toString + " " + cse.toString
    case InfinitiveIII(cse, voice) =>
      "Infinitive III " + voice.toString + " " + cse.toString
    case InfinitiveIV(cse) =>
      "Infinitive IV " + cse.toString
    case InfinitiveV =>
      "Infinitive V"
    case Participle(tempus, voice, declensionOpt) =>
      "Participle " + voice.toString + " " + tempus.toString + printDeclension(declensionOpt)
    case ParticipleAgent(mode, declensionOpt) =>
      "Agent Participle " + (if mode == Negative then " Negative" else "")  + printDeclension(declensionOpt)
  }

  def printSuffix(suffixOpt: Option[PossessiveSuffix]): String =
    import PossessiveSuffix._
    suffixOpt match {
      case Some(SingularFirst) => " , Possessive suffix: 1. Singular"
      case Some(SingularSecond) => " , Possessive suffix: 2. Singular"
      case Some(PluralFirst) => " , Possessive suffix: 1. Plural"
      case Some(PluralSecond) => " , Possessive suffix: 2. Plural"
      case Some(ThirdPos) => " , Possessive suffix: 3."
      case None         => ""
    }

  def printDeclension(declensionOpt: Option[NomineMorphemes]): String =
    declensionOpt match {
      case Some(declension) => " "  + declension.number.toString + " " + declension.cse.toString
      case None             => ""
    }
}

object FilePrint {
  import VerbMophemes._
  def apply(morphemes: Morphemes): String = morphemes match {
    case NomineMorphemes(cse, form) => printGrammaticalNumber(form) + ":" + printCase(cse)
    case Standard(modus, tempus, persona, mode) =>
      printModus(modus) + ":" + printTempus(tempus) + ":" + printPersona(persona) + printMode(mode)
    case InfinitiveI(Type.Short) =>
      "Inf1"
    case InfinitiveI(Type.Long) =>
      "Inf1:L"
    case InfinitiveII(cse, voice) =>
      "Inf2:" + printCase(cse) + ":" + printVoice(voice)
    case InfinitiveIII(cse, voice) =>
      "Inf3:" + printCase(cse) + ":" + printVoice(voice)
    case InfinitiveIV(cse) =>
      "Inf4:" + printCase(cse)
    case InfinitiveV =>
      "Inf5"
    case Participle(tempus, voice, declensionOpt) =>
      "Cip:" +printTempus(tempus) + ":" + printVoice(voice) + printDeclension(declensionOpt)
    case ParticipleAgent(mode, declensionOpt) =>
      "Cip:Age" + printMode(mode) + printDeclension(declensionOpt)
  }

  def printSuffix(suffixOpt: Option[PossessiveSuffix]): String =
    suffixOpt match {
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

  def printGrammaticalNumber(number: GNumber): String =
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
    }
}