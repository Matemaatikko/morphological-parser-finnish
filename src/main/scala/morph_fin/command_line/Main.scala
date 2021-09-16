package morph_fin.command_line

import scala.io.StdIn.readLine
import morph_fin.rulings.nouns.{Filters, NounPrinter}
import morph_fin.utils.TryOrElse

object Main extends App {

  var lemma: String = "valo"
  var filters: Filters = Filters()

  val printer = new NounPrinter()

  def printState =
    print(TryOrElse(printer.print(lemma, filters), ""))

  def printTypeCommand =
    println()
    print("Type>")

  def clear =
    new ProcessBuilder("cmd", "/c", "cls").inheritIO.start.waitFor

  //App

  clear
  while(true)
    printState
    printTypeCommand
    val (lemma_, filters_) = TryOrElse(ParseCommand(readLine()), ("NotFound", Filters()))
    lemma = lemma_
    filters = filters_
    clear


}
