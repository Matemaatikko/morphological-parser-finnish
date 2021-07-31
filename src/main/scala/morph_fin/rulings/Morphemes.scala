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

//Gradation

enum GradationType:
  case Strong, Weak, Unnecessary

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

case class NomineMorphemes(cse: Case, form: Form)

enum VerbMophemes:
  case Standard(voice: Voice, modus: Modus, tempus: Tempus, persona: Persona, mode: Mode)
  case InfinitiveI(tpe: Type)
  case InfinitiveII(cse: Case, voice: Voice)
  case InfinitiveIII(cse: Case, voice: Voice)
  case InfinitiveIV(cse: Case)
  case InfinitiveV
  case Participle(tempus: Tempus, voice: Voice)
  case ParticipleAgent(mode: Mode)

//TODO implement to string methods