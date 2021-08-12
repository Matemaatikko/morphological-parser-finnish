package morph_fin.rulings

object MorphemesUtils {

  object N {
    def apply(cse: Case, number: GNumber) = NomineMorphemes(cse, number)
  }

  object V {
    def apply(modus: Modus, tempus: Tempus, persona: Persona, mode: Mode) = VerbMophemes.Standard(modus, tempus, persona, mode)
  }

  val Nom = Nominative
  val Gen = Genitive
  val Par = Partitive
  val Acc = Accusative

  val Ine = Inessive
  val Ela = Elative
  val Ill = Illative

  val Ade = Adessive
  val Abl = Ablative
  val All = Allative

  val Ess = Essive
  val Tra = Translative
  val Abe = Abessive
  val Ins = Instructive
  val Com = Comitative

  val S = Singular
  val P = Plural

  extension (cse: Case)
    def ::(number: GNumber): NomineMorphemes = NomineMorphemes(cse, number)

  val NomS = Nom :: S
  val Inf1 = VerbMophemes.InfinitiveI(Type.Short)

  extension (morphemes: Morphemes)
    def ++(suffix: PossessiveSuffix): MorphemesWithPosSuffix = MorphemesWithPosSuffix(morphemes, suffix)

}
