package morph_fin.rulings

import morph_fin.rulings.PossessiveSuffix.{PluralFirst, PluralSecond, SingularFirst, SingularSecond}
import morph_fin.rulings.Print.printSuffix


//Nomine
sealed trait Case
  case object Nominative extends Case
  case object Genitive extends Case
  case object Accusative extends Case
  case object Partitive extends Case

  case object Inessive extends Case
  case object Elative extends Case
  case object Illative extends Case

  case object Adessive extends Case
  case object Ablative extends Case
  case object Allative extends Case

  case object Essive extends Case
  case object Translative extends Case
  case object Instructive extends Case
  case object Abessive extends Case
  case object Comitative extends Case

sealed trait GNumber
  case object Plural extends GNumber
  case object Singular extends GNumber

//Verb

sealed trait Mode
  case object Positive extends Mode
  case object Negative extends Mode

sealed trait Voice
  case object Active extends Voice
  case object Passive extends Voice

sealed trait Modus
  case object Indicative extends Modus
  case object Conditional extends Modus
  case object Potential extends Modus
  case object Imperative extends Modus

sealed trait Tempus
  case object Present extends Tempus
  case object Imperfect extends Tempus
  case object Perfect extends Tempus
  case object Pluperfect extends Tempus

sealed trait Person
  case object First extends Person
  case object Second extends Person
  case object Third extends Person


enum Persona:
  case Active(number: GNumber, person: Person)
  case Passive


//Only for infinitive
enum Type:
  case Short, Long


enum PossessiveSuffix:
  case SingularFirst, SingularSecond
  case PluralFirst, PluralSecond
  case ThirdPos

//Morphemes

trait Morphemes

case class NomineMorphemes(cse: Case, number: GNumber) extends Morphemes
case class MorphemesWithPosSuffix(morphemes: Morphemes, suffix: PossessiveSuffix) extends Morphemes

enum VerbMophemes extends Morphemes:
  case Standard(modus: Modus, tempus: Tempus, persona: Persona, mode: Mode)
  case InfinitiveI(tpe: Type)
  case InfinitiveII(cse: Case, voice: Voice)
  case InfinitiveIII(cse: Case, voice: Voice)
  case InfinitiveIV(cse: Case)
  case InfinitiveV
  case Participle(tempus: Tempus, voice: Voice, declension: Option[NomineMorphemes] = None)
  case ParticipleAgent(mode: Mode, declensionOpt: Option[NomineMorphemes] = None)
