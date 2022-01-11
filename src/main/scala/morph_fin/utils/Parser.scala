package morph_fin.utils

import scala.annotation.tailrec
import scala.collection.mutable

class Parser(stream: Iterator[Char]) {

  val streamEnded = '§'
  var loadedCharacters = List[Char]()

  private def append =
    if(stream.hasNext) loadedCharacters :+= stream.next()
    else loadedCharacters :+= streamEnded

  def peek: Char =
    if(loadedCharacters.isEmpty) append
    loadedCharacters.head

  def peek2: (Char, Char) =
    while(loadedCharacters.length < 2) append
    (loadedCharacters(0), loadedCharacters(1))

  def peek3: (Char, Char, Char) =
    while(loadedCharacters.length < 3) append
    (loadedCharacters(0), loadedCharacters(1), loadedCharacters(2))

  def consume: Char =
    val last = peek
    loadedCharacters = loadedCharacters.drop(1)
    last

  def skipTimes(times: Int) =
    (0 until times).foreach(_ => consume)

  def skip(value: Char, error: String = ""): Unit =
    if peek != value then throw new Exception(error + peek)
    consume

  def skipUntil(condition: => Boolean, error: String = ""): Unit =
    if !condition then
      consume
      skipUntil(condition, error)

  def skipAll(value: String, error: String = ""): Unit =
    value.foreach(c => skip(c, error))

  def skipWhiteSpaces =
    while (peek.isWhitespace) consume

  def skipWhiteSpacesUnlessLineBreak =
    while (peek.isWhitespace && peek != '\n')  consume

  def collectUntil(condition: => Boolean): String =
    @tailrec
    def iter(result: String): String =
      if(!condition && peek != streamEnded) iter(result + consume)
      else result
    iter("")

  def doUntil[A](fun: => A, condition: => Boolean): Seq[A] =
    @tailrec
    def iter(result: Seq[A]): Seq[A] =
      if(!condition && peek != streamEnded) iter(result :+ fun)
      else result
    iter(Nil)

  def hasEnded: Boolean = peek == streamEnded
}
