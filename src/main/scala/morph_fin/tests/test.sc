import morph_fin.inflection.EndingSeparator
import morph_fin.rulings.nouns.{Filters, NounPrinter, StructuredWord}


//val printer = new NounPrinter()


//print(printer.print("vesi", Filters()))

val separator = new EndingSeparator()
separator.registerAll

println(separator.words.length)

println(separator.endingList.head.list.size)
println(separator.endingList.drop(1).head.list.size)
println(separator.endingList.size)

println(separator.endingList.map(_.list.mkString(",")).mkString("\n"))
