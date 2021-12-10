package morph_fin.rulings.morpheme

import morph_fin.rulings.morpheme.{Case, GrammaticalNumber, Morpheme}
import morph_fin.rulings.*

sealed trait Morphemes {

  def ~(morpheme: Morpheme) = Append(morpheme, this)
  private def isImpl(morpheme: Morpheme): Boolean = this match {
    case Append(_moprheme, _morphemes) => morpheme == _moprheme || _morphemes.isImpl(morpheme)
    case _ => false
  }

  def is(_morphemes: Morpheme*): Boolean = _morphemes.forall(this.isImpl(_))
  def is(_morphemes: Morphemes): Boolean = _morphemes match {
    case Append(moprheme, tail) => this.is(moprheme) && this.is(tail)
    case a: Root => a == this.root
  }

  def isNot(_morphemes: Morpheme*): Boolean =  _morphemes.exists(!this.isImpl(_))
  def isNot(_morphemes: Morphemes): Boolean = _morphemes match {
    case Append(moprheme, tail) => this.isNot(moprheme) && this.isNot(tail)
    case a: Root => a != this.root
  }

  def isNotPossessiveSuffix: Boolean = isNotAny(
    PossessiveSuffix.SingularFirst,
    PossessiveSuffix.SingularSecond,
    PossessiveSuffix.PluralFirst,
    PossessiveSuffix.PluralSecond,
    PossessiveSuffix.ThirdPos,
  )

  def isNotComparativeForm: Boolean = isNotAny(Comparative, Superlative)

  def isNotAny(_morphemes: Morpheme*): Boolean =  _morphemes.forall(!this.isImpl(_))

  def root: Root = this match {
    case Append(_, _morph) => _morph.root
    case a: Root => a
  }

  def getCase: Option[Case] = this match {
    case Append(cse: Case, rest) => Some(cse)
    case Append(_, rest)         => rest.getCase
    case _                       => None
  }

  def getGrammaticalNumber: Option[GrammaticalNumber] = this match {
    case Append(value: GrammaticalNumber, rest) => Some(value)
    case Append(_, rest)         => rest.getGrammaticalNumber
    case _                       => None
  }

  def updateRoot(root: Root): Morphemes = this match {
    case Append(value, rest) => Append(value, rest.updateRoot(root))
    case r: Root => root
    case a => a
  }
}
sealed trait Root extends Morphemes

sealed trait Verb extends Root
case object Finite extends Verb

sealed trait Infinitive extends Verb
case object AInfinitive extends Infinitive
case object AInfinitiveLong extends Infinitive
case object EInfinitive extends Infinitive
case object MAInfinitive extends Infinitive
case object InfinitiveIV extends Infinitive
case object InfinitiveV extends Infinitive
case object InfinitiveVI extends Infinitive
case object InfinitiveVII extends Infinitive

sealed trait Participle extends Verb
case object vAParticiple extends Participle
case object nUtParticiple extends Participle
case object tUParticiple extends Participle
case object AgentParticiple extends Participle
case object NegativeParticiple extends Participle


case object Noun extends Root
case class Append(moprheme: Morpheme, morphemes: Morphemes) extends Morphemes
