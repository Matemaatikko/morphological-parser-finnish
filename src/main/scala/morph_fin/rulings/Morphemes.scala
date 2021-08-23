package morph_fin.rulings

import morph_fin.rulings.PossessiveSuffix.{PluralFirst, PluralSecond, SingularFirst, SingularSecond}


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

sealed trait Comparation
  case object Comparative extends Comparation
  case object Superlative extends Comparation


enum Persona:
  case Active(number: GNumber, person: Person)
  case Passive

enum PossessiveSuffix:
  case Body
  case SingularFirst, SingularSecond
  case PluralFirst, PluralSecond
  case ThirdPos


//Morphemes

trait Morphemes

case class NomineMorphemes(cse: Case, number: GNumber) extends Morphemes
case class ComparationMorphemes(comparation: Comparation, nomineMorphemes: NomineMorphemes) extends Morphemes
case object stiAdverb extends Morphemes

case class MorphemesWithPosSuffix(morphemes: Morphemes, suffix: PossessiveSuffix) extends Morphemes

sealed trait VerbMophemes extends Morphemes
  case class Standard(modus: Modus, tempus: Tempus, persona: Persona, mode: Mode) extends VerbMophemes
  case class AInfinitive(long: Boolean)  extends VerbMophemes
  case class EInfinitive(cse: Case, voice: Voice) extends VerbMophemes
  case class MAInfinitive(cse: Case, voice: Voice) extends VerbMophemes
  case object InfinitiveIV extends VerbMophemes
  case object InfinitiveV extends VerbMophemes
  case object InfinitiveVI extends VerbMophemes
  case object InfinitiveVII extends VerbMophemes

sealed trait Participle extends VerbMophemes
  case class vAParticiple(voice: Voice) extends Participle
  case object nUtParticiple extends Participle
  case object tUParticiple extends Participle
  case object AgentParticiple extends Participle
  case object NegativeParticiple extends Participle
