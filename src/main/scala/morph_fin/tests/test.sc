import morph_fin.inflection.EndingSeparator
import morph_fin.kotus_format.{LoadUpdatedKotus, ReformatKotus, UpdatedWord}
import morph_fin.rulings.nouns.{Filters, NounPrinter, StructuredWord, Word}
import morph_fin.rulings.pronouns.LoadPronounInflections
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNomineRules}
import morph_fin.rulings.rules.*

//ReformatKotus.reformat



println(LoadPronounInflections.apply().map(a => (a.lemma, a.inflections.mkString("\n"))).mkString("\n\n"))

//val printer = new NounPrinter()
//print(printer.print("kolme", Filters()))


//given Seq[DeclensionRule] = LoadAndParseNomineRules.rules
//
//val words= LoadUpdatedKotus.apply()
//  .filter(_.isInstanceOf[UpdatedWord.Compound])
//  .map(_.asInstanceOf[UpdatedWord.Compound])
//  .filter(_.suffix.inflection.rule == 99)
//  .mkString("\n")
//
//println(words.toString())

//val separator = new EndingSeparator()
//separator.registerAll

