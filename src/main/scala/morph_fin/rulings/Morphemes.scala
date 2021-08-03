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

//TODO implement to string methods

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