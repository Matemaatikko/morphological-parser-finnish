package morph_fin.rulings.morpheme

import morph_fin.rulings.morpheme.PossessiveSuffix.{PluralFirst, PluralSecond, SingularFirst, SingularSecond}

object MorphemeOrdering {

  val valueMap: Map[Morpheme, Int] = Map(
    Nominative  -> 1,
    Genitive    -> 2,
    Accusative  -> 3,
    Partitive   -> 4,
    Inessive    -> 5,
    Elative     -> 6,
    Illative    -> 7,
    Adessive    -> 8,
    Ablative    -> 9,
    Allative    -> 10,
    Essive      -> 11,
    Translative -> 12,
    Instructive -> 13,
    Abessive    -> 14,
    Comitative  -> 15,

    Singular -> 100,
    Plural   -> 200,

    //TODO change the following values.

    Positive -> 100,
    Negative -> 200,

    Active -> 100,
    Passive -> 200,

    Indicative  -> 100,
    Conditional -> 200,
    Potential   -> 300,
    Imperative  -> 400,

    Present    -> 100,
    Imperfect  -> 200,
    Perfect    -> 300,
    Pluperfect -> 400,

    General -> 100,

    SingularFirst  -> 100,
    SingularSecond -> 200,
    SingularThird  -> 300,
    PluralFirst  -> 400,
    PluralSecond -> 500,
    PluralThird  -> 600,

    Comparative -> 100,
    Superlative -> 200,

    PossessiveSuffix.Body           -> 100,
    PossessiveSuffix.SingularFirst  -> 200,
    PossessiveSuffix.SingularSecond -> 300,
    PossessiveSuffix.PluralFirst    -> 400,
    PossessiveSuffix.PluralSecond   -> 500,
    PossessiveSuffix.ThirdPos       -> 600,

    stiAdverb -> 1000
  )



}
