import morph_fin.rulings.nomines.{LoadAndParseNomineRules, RepChar}

val nomineRulings = LoadAndParseNomineRules.rules

def toStr(c: RepChar): String =
  c match {
    case RepChar.Ch(c) => c.toString
    case RepChar.Rep(c) => "_" + c.toString + "_"
  }

val result0 = nomineRulings.map(a => a.number.toString + ":" + a.drop.toString + ":\n"
  + a.cases.mkString("\n"))
  .mkString("\n=============================================\n")
val result = nomineRulings.map(a => a.number.toString + ":" + a.drop.toString + ":\n"
  + a.cases.map(a => "-"+ a.ending.map(toStr(_)).mkString(""))
  .mkString("\n")).mkString("\n=============================================\n")

print(result)