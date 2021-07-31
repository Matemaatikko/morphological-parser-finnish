package morph_fin.rulings.verbs

import morph_fin.rulings.{Form, Mode, Modus, Persona, PersonaNumber, Tempus, VerbMophemes, Voice}

object VerbMappings {

  val activeSingular = VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Imperfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Negative)
  val activePlural = VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Imperfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Negative)
  val passive  = VerbMophemes.Standard(Voice.Passive, Modus.Indicative, Tempus.Imperfect, Persona.Passive, Mode.Negative)

  def list: Seq[(VerbMophemes, VerbMophemes)] = Seq(
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Indicative, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Indicative, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Indicative, Tempus.Pluperfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Indicative, Tempus.Pluperfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Indicative, Tempus.Pluperfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Conditional, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Conditional, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Conditional, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Positive) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Potential, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.First), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Second), Mode.Negative) -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.First), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Second), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Active, Modus.Potential, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Potential, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,

    //---------------------------

    VerbMophemes.Standard(Voice.Active, Modus.Imperative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Positive)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Imperative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Positive)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Imperative, Tempus.Perfect, Persona.Passive, Mode.Positive)  -> passive,

    VerbMophemes.Standard(Voice.Active, Modus.Imperative, Tempus.Perfect, Persona.Active(Form.Singular, PersonaNumber.Third), Mode.Negative)  -> activeSingular,
    VerbMophemes.Standard(Voice.Active, Modus.Imperative, Tempus.Perfect, Persona.Active(Form.Plural, PersonaNumber.Third), Mode.Negative)  -> activePlural,
    VerbMophemes.Standard(Voice.Passive, Modus.Imperative, Tempus.Perfect, Persona.Passive, Mode.Negative)  -> passive,
  )

}
