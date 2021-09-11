package morph_fin.rulings

sealed trait Morphemes
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

extension (morphemes: Morphemes)
  def ~(morpheme: Morpheme) = Append(morpheme, morphemes)
  private def isImpl(morpheme: Morpheme): Boolean = morphemes match {
    case Append(_moprheme, _morphemes) => morpheme == _moprheme || _morphemes.isImpl(morpheme)
    case _ => false
  }

  def is(_morphemes: Morpheme*): Boolean = _morphemes.forall(morphemes.isImpl(_))
  def is(_morphemes: Morphemes): Boolean = _morphemes match {
    case Append(moprheme, tail) => morphemes.is(moprheme) && morphemes.is(tail)
    case a: Root => a == morphemes.root
  }

  def isNot(_morphemes: Morpheme*): Boolean =  _morphemes.exists(!morphemes.isImpl(_))
  def isNot(_morphemes: Morphemes): Boolean = _morphemes match {
    case Append(moprheme, tail) => morphemes.isNot(moprheme) || morphemes.isNot(tail)
    case a: Root => a != morphemes.root
  }

  def root: Root = morphemes match {
    case Append(_, _morph) => _morph.root
    case a: Root => a
  }

end extension