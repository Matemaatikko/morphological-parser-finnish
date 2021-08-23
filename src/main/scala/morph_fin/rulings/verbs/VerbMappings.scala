package morph_fin.rulings.verbs

import morph_fin.rulings._

object VerbMappings {

  val activeSingular = Standard(Indicative, Imperfect, Persona.Active(Singular, First), Negative)
  val activePlural = Standard(Indicative, Imperfect, Persona.Active(Plural, First), Negative)
  val passive  = Standard(Indicative, Imperfect, Persona.Passive, Negative)

  def list: Seq[(VerbMophemes, VerbMophemes)] = Seq(
    Standard(Indicative, Perfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    Standard(Indicative, Perfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    Standard(Indicative, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    Standard(Indicative, Perfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    Standard(Indicative, Perfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    Standard(Indicative, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    Standard(Indicative, Perfect, Persona.Passive, Positive)  -> passive,

    Standard(Indicative, Perfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    Standard(Indicative, Perfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    Standard(Indicative, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    Standard(Indicative, Perfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    Standard(Indicative, Perfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    Standard(Indicative, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    Standard(Indicative, Perfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    Standard(Indicative, Pluperfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    Standard(Indicative, Pluperfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    Standard(Indicative, Pluperfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    Standard(Indicative, Pluperfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    Standard(Indicative, Pluperfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    Standard(Indicative, Pluperfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    Standard(Indicative, Pluperfect, Persona.Passive, Positive)  -> passive,

    Standard(Indicative, Pluperfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    Standard(Indicative, Pluperfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    Standard(Indicative, Pluperfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    Standard(Indicative, Pluperfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    Standard(Indicative, Pluperfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    Standard(Indicative, Pluperfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    Standard(Indicative, Pluperfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    Standard(Conditional, Perfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    Standard(Conditional, Perfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    Standard(Conditional, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    Standard(Conditional, Perfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    Standard(Conditional, Perfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    Standard(Conditional, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    Standard(Conditional, Perfect, Persona.Passive, Positive)  -> passive,

    Standard(Conditional, Perfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    Standard(Conditional, Perfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    Standard(Conditional, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    Standard(Conditional, Perfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    Standard(Conditional, Perfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    Standard(Conditional, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    Standard(Conditional, Perfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    Standard(Potential, Perfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    Standard(Potential, Perfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    Standard(Potential, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    Standard(Potential, Perfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    Standard(Potential, Perfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    Standard(Potential, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    Standard(Potential, Perfect, Persona.Passive, Positive)  -> passive,

    Standard(Potential, Perfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    Standard(Potential, Perfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    Standard(Potential, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    Standard(Potential, Perfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    Standard(Potential, Perfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    Standard(Potential, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    Standard(Potential, Perfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    Standard(Imperative, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    Standard(Imperative, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    Standard(Imperative, Perfect, Persona.Passive, Positive)  -> passive,

    Standard(Imperative, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    Standard(Imperative, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    Standard(Imperative, Perfect, Persona.Passive, Negative)  -> passive,
  )

}
