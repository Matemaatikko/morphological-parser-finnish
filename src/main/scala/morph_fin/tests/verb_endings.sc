import morph_fin.rulings.nomines.RepChar
import morph_fin.rulings.verbs.LoadAndParseVerbRules

val rules = LoadAndParseVerbRules.rules

def toStr(c: RepChar): String =
  c match {
    case RepChar.Ch(c) => c.toString
    case RepChar.Rep(c) => "_" + c.toString + "_"
  }

val result0 = rules.map(a => a.ruleNumber.toString + ":" + a.drop.toString + ":\n"
  + a.cases.mkString("\n"))
  .mkString("\n=============================================\n")
val result = rules.map(a => a.ruleNumber.toString + ":" + a.drop.toString + ":\n"
  + a.cases.map(a => "-"+ a.ending.map(toStr(_)).mkString(""))
  .mkString("\n")).mkString("\n=============================================\n")

print(result)