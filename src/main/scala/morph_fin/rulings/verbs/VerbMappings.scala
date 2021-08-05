package morph_fin.rulings.verbs

import morph_fin.rulings.{GNumber, Mode, Modus, Persona, Person, Tempus, VerbMophemes, Voice}

object VerbMappings {

  val activeSingular = VerbMophemes.Standard(Modus.Indicative, Tempus.Imperfect, Persona.Active(GNumber.Singular, Person.First), Mode.Negative)
  val activePlural = VerbMophemes.Standard(Modus.Indicative, Tempus.Imperfect, Persona.Active(GNumber.Plural, Person.First), Mode.Negative)
  val passive  = VerbMophemes.Standard(Modus.Indicative, Tempus.Imperfect, Persona.Passive, Mode.Negative)

  def list: Seq[(VerbMophemes, VerbMophemes)] = Seq(
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Singular, Person.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Plural, Person.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Singular, Person.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Plural, Person.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Indicative, Tempus.Pluperfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Conditional, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Potential, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Modus.Imperative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Modus.Imperative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Modus.Imperative, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Modus.Imperative, Tempus.Perfect, Persona.Active(GNumber.Singular, Person.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Modus.Imperative, Tempus.Perfect, Persona.Active(GNumber.Plural, Person.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Modus.Imperative, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,
  )

}
