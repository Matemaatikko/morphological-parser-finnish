package morph_fin.rulings


//Nomine
enum Case:
  case Nominative, Genetive, Akkusative, Partitive
  case Inessive, Elative, Illative
  case Adessive, Ablative, Allative
  case Essive, Translative, Instructive, Abessive, Komitative
end Case

enum Form:
  case Plural, Singular

//Verb

enum Mode:
  case Positive, Negative

enum Voice:
  case Active, Passive

enum Modus:
  case Indicative, Conditional, Potential, Imperative

enum Tempus:
  case Presens, Imperfect, Perfect, Pluperfect

enum PersonaNumber:
  case First, Second, Third


enum Persona:
  case Active(form: Form, number: PersonaNumber)
  case Passive


//Only for infinitive
enum Type:
  case Short, Long


//Morphemes

trait Morphemes

case class NomineMorphemes(cse: Case, form: Form) extends Morphemes

enum VerbMophemes extends Morphemes:
  case Standard(modus: Modus, tempus: Tempus, persona: Persona, mode: Mode)
  case InfinitiveI(tpe: Type)
  case InfinitiveII(cse: Case, voice: Voice)
  case InfinitiveIII(cse: Case, voice: Voice)
  case InfinitiveIV(cse: Case)
  case InfinitiveV
  case Participle(tempus: Tempus, voice: Voice)
  case ParticipleAgent(mode: Mode)

object Print {
  import VerbMophemes._
  def apply(morphemes: Morphemes): String = morphemes match {
    case NomineMorphemes(cse, form) => cse.toString + " " + form.toString
    case Standard(modus, tempus, Persona.Passive, mode) =>
      modus.toString + " " + tempus.toString + " Passive" + (if mode == Mode.Negative then " Negative" else "")
    case Standard(modus, tempus, Persona.Active(form, number), mode) =>
      modus.toString + " " + tempus.toString + " Active " + form.toString + " " + number.toString + (if mode == Mode.Negative then " Negative" else "")
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
    case Participle(tempus, voice) =>
      "Participle " + voice.toString + " " + tempus.toString
    case ParticipleAgent(mode) =>
      "Agent Participle " + (if mode == Mode.Negative then " Negative" else "")
  }
}

object FilePrint {
  import VerbMophemes._
  def apply(morphemes: Morphemes): String = morphemes match {
    case NomineMorphemes(cse, form) => printForm(form) + ":" + printCase(cse)
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
    case Participle(tempus, voice) =>
      "Cip:" +printTempus(tempus) + ":" + printVoice(voice)
    case ParticipleAgent(mode) =>
      "Cip:Age" + printMode(mode)
  }

  def printCase(cse: Case): String =
    import Case._
    cse match {
      case Nominative  => "Nom"
      case Genetive    => "Gen"
      case Akkusative  => "Akk"
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
      case Komitative  => "Kom"
    }

  def printForm(form: Form): String =
    import Form._
    form match {
      case Singular => "S"
      case Plural   => "P"
    }

  def printModus(modus: Modus): String =
    import Modus._
    modus match {
      case Indicative  => "Ind"
      case Conditional => "Con"
      case Potential   => "Pot"
      case Imperative  => "Imp"
    }

  def printTempus(tempus: Tempus): String =
    import Tempus._
    tempus match {
      case Presens     => "Pre"
      case Imperfect   => "Imp"
      case Perfect     => "Per"
      case Pluperfect  => "Plp"
    }

  def printPersona(persona: Persona): String =
    import Persona._
    persona match {
      case Passive              => "Pas"
      case Active(form, number) => "Akt:" + printForm(form) + printNumber(number)
    }

  def printNumber(personaNumber: PersonaNumber): String =
    import PersonaNumber._
    personaNumber match {
      case First  => "1"
      case Second => "2"
      case Third  => "3"
    }

  def printMode(mode: Mode): String =
    import Mode._
    mode match {
      case Positive  => ""
      case Negative  => ":Neg"
    }

  def printVoice(voice: Voice): String =
    import Voice._
    voice match {
      case Active    => "Akt"
      case Passive   => "Pas"
    }
}