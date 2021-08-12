package morph_fin.rulings.verbs

import morph_fin.rulings._

object VerbMappings {

  val activeSingular = VerbMophemes.Standard(Indicative, Imperfect, Persona.Active(Singular, First), Negative)
  val activePlural = VerbMophemes.Standard(Indicative, Imperfect, Persona.Active(Plural, First), Negative)
  val passive  = VerbMophemes.Standard(Indicative, Imperfect, Persona.Passive, Negative)

  def list: Seq[(VerbMophemes, VerbMophemes)] = Seq(
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Passive, Positive)  -> passive,

    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    VerbMophemes.Standard(Indicative, Perfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Passive, Positive)  -> passive,

    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    VerbMophemes.Standard(Indicative, Pluperfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Passive, Positive)  -> passive,

    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    VerbMophemes.Standard(Conditional, Perfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Singular, First), Positive) -> activeSingular,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Singular, Second), Positive) -> activeSingular,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Plural, First), Positive)  -> activePlural,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Plural, Second), Positive)  -> activePlural,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    VerbMophemes.Standard(Potential, Perfect, Persona.Passive, Positive)  -> passive,

    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Singular, First), Negative) -> activeSingular,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Singular, Second), Negative) -> activeSingular,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Plural, First), Negative)  -> activePlural,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Plural, Second), Negative)  -> activePlural,
    VerbMophemes.Standard(Potential, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    VerbMophemes.Standard(Potential, Perfect, Persona.Passive, Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Imperative, Perfect, Persona.Active(Singular, Third), Positive)  -> activeSingular,
    VerbMophemes.Standard(Imperative, Perfect, Persona.Active(Plural, Third), Positive)  -> activePlural,
    VerbMophemes.Standard(Imperative, Perfect, Persona.Passive, Positive)  -> passive,

    VerbMophemes.Standard(Imperative, Perfect, Persona.Active(Singular, Third), Negative)  -> activeSingular,
    VerbMophemes.Standard(Imperative, Perfect, Persona.Active(Plural, Third), Negative)  -> activePlural,
    VerbMophemes.Standard(Imperative, Perfect, Persona.Passive, Negative)  -> passive,
  )

}
