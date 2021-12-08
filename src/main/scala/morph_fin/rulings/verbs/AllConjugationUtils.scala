package morph_fin.rulings.verbs

import morph_fin.rulings.morpheme.*
import morph_fin.rulings.nouns.{AllDeclensionUtils, InflectedWord, PossessiveSuffixUtils, Word}
import morph_fin.rulings.rules.{ConjugationRule, DeclensionRule, Gradation}
import morph_fin.rulings.verbs.ConjugationUtils.generateConjugations
import PossessiveSuffixUtils.*

object AllConjugationUtils {

  //Note exception cases: 77 and 78.
  def generateAllConjugations(word: Word)(using rules: Seq[ConjugationRule], rules2: Seq[DeclensionRule]): Seq[InflectedWord] =
    val cases = generateConjugations(word)

    //Infinitives receiving possessive suffixes
    val Inf1Long = cases.find(_.morphemes.root == AInfinitiveLong).get
    val Inf2IneAct = cases.find(a => a.morphemes.root == EInfinitive && a.morphemes.is(Inessive, Active)).get //Optional possessivesuffix
    val Inf5 = cases.find(_.morphemes.root == InfinitiveV).get
    val Inf6 = cases.find(_.morphemes.root == InfinitiveVI).get

    //Infinitive IV
    val Inf4 = cases.find(_.morphemes.root == InfinitiveIV).get

    //Participles
    val vaParAct = cases.find(a => a.morphemes.root == vAParticiple && a.morphemes.is(Active)).get
    val vaParPas = cases.find(a => a.morphemes.root == vAParticiple && a.morphemes.is(Passive)).get
    val nutPar = cases.find(_.morphemes.root == nUtParticiple).get
    val tuPar = cases.find(_.morphemes.root == tUParticiple).get
    val agePar = cases.find(_.morphemes.root == AgentParticiple).get
    val ageNegPar = cases.find(_.morphemes.root == NegativeParticiple).get

    val set = Seq(Inf1Long, Inf2IneAct, Inf5, Inf6, Inf4, vaParAct, vaParPas, nutPar, tuPar, ageNegPar, agePar)

    val filtered = cases.filterNot(a => set.contains(a))

    val addedSuffixes = onlySuffixes(Inf1Long) ++ addSuffixes(Inf2IneAct) ++ onlySuffixes(Inf5) ++ onlySuffixes(Inf6)

    val Inf4Result = AllDeclensionUtils.generateAllDeclections(Word(Inf4.word.toString, 38)).map(update(_, InfinitiveIV))
    val vaParActResult = AllDeclensionUtils.generateAllDeclections(Word(vaParAct.word.toString, 10)).map(update(_, vAParticiple)).map(append(_, Active))
    val vaParPasResult = AllDeclensionUtils.generateAllDeclections(Word(vaParPas.word.toString, 10)).map(update(_, vAParticiple)).map(append(_, Passive))

    val nutParResult = AllDeclensionUtils.generateAllDeclections(Word(nutPar.word.toString, 47)).map(update(_, nUtParticiple))
    val tuParResult = AllDeclensionUtils.generateAllDeclections(Word(tuPar.word.toString, 1, Some(Gradation("tt", "t")))).map(update(_, tUParticiple))
    val ageParResult = AllDeclensionUtils.generateAllDeclections(Word(agePar.word.toString, 10)).map(update(_, AgentParticiple))
    val ageNegParResult = AllDeclensionUtils.generateAllDeclections(Word(ageNegPar.word.toString, 34, Some(Gradation("tt", "t")))).map(update(_, NegativeParticiple))

    filtered ++ addedSuffixes ++ Inf4Result
      ++ vaParActResult ++ vaParPasResult
      ++ nutParResult ++ tuParResult
      ++ ageParResult ++ ageNegParResult

  end generateAllConjugations

  inline def update(word: InflectedWord, root: Root): InflectedWord =
    val updated = word.morphemes.updateRoot(root)
    word.copy(morphemes = updated)

  inline def append(word: InflectedWord, morpheme: Morpheme): InflectedWord =
    val updated = word.morphemes ~ morpheme
    word.copy(morphemes = updated)
}
