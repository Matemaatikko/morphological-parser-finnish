import morph_fin.inflection.EndingSeparator
import morph_fin.kotus_format.{LoadUpdatedKotus, UpdatedWord}
import morph_fin.rulings.nouns.{Filters, NounPrinter, StructuredWord, Word}
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNomineRules}
import morph_fin.rulings.rules._


val printer = new NounPrinter()
print(printer.print("kaunis", Filters()))


//given Seq[DeclensionRule] = LoadAndParseNomineRules.rules
//
//val words= LoadUpdatedKotus.apply()
//  .filter(_.isInstanceOf[UpdatedWord.Compound2])
//  .mkString("\n")
//
//println(words.toString())

//val separator = new EndingSeparator()
//separator.registerAll

