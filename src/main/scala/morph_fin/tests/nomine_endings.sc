import morph_fin.rulings.MorphemesUtils.*
import morph_fin.rulings.rules.{LoadAndParseNounRules, RepChar}

val nomineRulings = LoadAndParseNounRules.rules

def toStr(c: RepChar): String =
  c match {
    case RepChar.Ch(c) => c.toString
    case RepChar.Rep(c) => "_" + c.toString + "_"
  }

val result0 = nomineRulings.map(a => a.ruleNumber.toString + ":" + a.drop.toString + ":\n"
  + a.cases.mkString("\n"))
  .mkString("\n=============================================\n")
val result = nomineRulings.map(a => a.ruleNumber.toString + ":" + a.drop.toString + ":\n"
  + a.cases.map(a => "-"+ a.ending.map(toStr(_)).mkString(""))
  .mkString("\n")).mkString("\n=============================================\n")
val resultNP = nomineRulings.map(a => a.ruleNumber.toString + ":" + a.drop.toString + ":\n"
  + a.cases.filter(_.morphemes == Nom::P).map(a => "-"+ a.ending.map(toStr(_)).mkString(""))
  .mkString("\n")).mkString("\n=============================================\n")

print(result)