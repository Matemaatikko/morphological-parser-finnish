package morph_fin.rulings

import morph_fin.rulings.PossessiveSuffix.{PluralFirst, PluralSecond, SingularFirst, SingularSecond}

trait Morpheme
//Noun
sealed trait Case extends Morpheme
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

sealed trait GrammaticalNumber extends Morpheme
  case object Singular extends GrammaticalNumber
  case object Plural extends GrammaticalNumber

//Verb

sealed trait Mode extends Morpheme
  case object Positive extends Mode
  case object Negative extends Mode

sealed trait Voice extends Morpheme
  case object Active extends Voice
  case object Passive extends Voice

sealed trait Modus extends Morpheme
  case object Indicative extends Modus
  case object Conditional extends Modus
  case object Potential extends Modus
  case object Imperative extends Modus

sealed trait Tempus extends Morpheme
  case object Present extends Tempus
  case object Imperfect extends Tempus
  case object Perfect extends Tempus
  case object Pluperfect extends Tempus

sealed trait Person extends Morpheme
  case object SingularFirst extends Person
  case object SingularSecond extends Person
  case object SingularThird extends Person

  case object PluralFirst extends Person
  case object PluralSecond extends Person
  case object PluralThird extends Person

sealed trait Comparation extends Morpheme
  case object Comparative extends Comparation
  case object Superlative extends Comparation

enum PossessiveSuffix extends Morpheme:
  case Body
  case SingularFirst, SingularSecond
  case PluralFirst, PluralSecond
  case ThirdPos

case object stiAdverb extends Morpheme

