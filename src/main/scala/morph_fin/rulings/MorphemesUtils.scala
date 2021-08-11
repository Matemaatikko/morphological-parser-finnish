package morph_fin.rulings

object MorphemesUtils {

  import Case._
  sealed trait Case2(val cse: Case)
  case object Nom extends Case2(Nominative)
  case object Gen extends Case2(Genitive)
  case object Par extends Case2(Partitive)
  case object Acc extends Case2(Accusative)

  case object Ine extends Case2(Inessive)
  case object Ela extends Case2(Elative)
  case object Ill extends Case2(Illative)

  case object Ade extends Case2(Adessive)
  case object Abl extends Case2(Ablative)
  case object All extends Case2(Allative)

  case object Ess extends Case2(Essive)
  case object Tra extends Case2(Translative)
  case object Abe extends Case2(Abessive)
  case object Ins extends Case2(Instructive)
  case object Com extends Case2(Comitative)

  import GNumber._
  sealed trait GNumber2(val number: GNumber)
  case object P extends GNumber2(Plural)
  case object S extends GNumber2(Singular)

  extension (value: Case2)
    def ::(number: GNumber2): NomineMorphemes = NomineMorphemes(value.cse, number.number)

  val NomS = Nom :: S
  val Inf1 = VerbMophemes.InfinitiveI(Type.Short)

  //TODO remake to take account other morpheme cases.
  extension (morphemes: Morphemes)
    def ++(suffix: PossessiveSuffix): NomineMorphemes =
      val a = morphemes.asInstanceOf[NomineMorphemes]
      a.copy(suffixOpt = Some(suffix))
}
