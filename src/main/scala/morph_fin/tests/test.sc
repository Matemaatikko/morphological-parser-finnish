import morph_fin.*
import morph_fin.inflection.GenerateInflectedWords
import morph_fin.kotus_format.*
import morph_fin.kotus_format.PrintUpdatedWord.{bendingString, toString}
import morph_fin.kotus_format.UpdatedWord.{Compound, Compound2, Error, NoBending, Prefix, Pronoun, StandardBending, Suffix, SuffixError}
import morph_fin.rulings.{Print, *}
import morph_fin.rulings.nomines.{DeclensionUtils, GenerateDeclensionRules, Gradation, LoadAndParseNomineRules, NomineRulesParser, ResultWord, Word}
import morph_fin.rulings.verbs.{ConjugationUtils, GenerateConjugationRules, LoadAndParseVerbRules, VerbRulesParser}
import morph_fin.utils.{FilesLocation, Hyphenation}

import java.io.File
import scala.collection.mutable
import scala.io.{Codec, Source}
