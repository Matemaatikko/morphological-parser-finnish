package morph_fin.command_line

import morph_fin.rulings.nouns.Filters
import morph_fin.utils.Parser
import morph_fin.utils.TryOrElse

object ParseCommand{
  def apply(command: String): (String, Filters) = new CommandParser(command.iterator).parse
}

class CommandParser(stream: Iterator[Char]) extends Parser(stream){

  def parse: (String, Filters) =
    skipWhiteSpaces
    val lemma = collectUntil(peek == ' ')
    skipWhiteSpaces
    val config = collectUntil(peek == ' ')
    val filters = TryOrElse(Filters(
      bol(config(0)),
      bol(config(1)),
      bol(config(2)),
      bol(config(3)),
      bol(config(4)),
      bol(config(5)),
    ), Filters())
    (lemma, filters)

  def bol(char: Char): Boolean = char == '1'
}
