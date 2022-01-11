package morph_fin.parser

case class State[A](map: Map[Char, State[A]], values: Seq[A])
def Empty[A] = State[A](Map(), Nil)


extension[A] (s: State[A])
  //def print: String =  s.value.toString + "\n" + s.map.toList.map(tuple => tuple._1 + "->" + tuple._2.print).mkString("[","\n", "]")
  def find(path: String): Seq[A] =
    if path.isEmpty then s.values
    else
      s.map.get(path.head).map(_.find(path.tail)).getOrElse(Nil)

  def traverse(path: String): Seq[(A, String)] =
    if path.isEmpty then s.values.map(v => v -> path)
    else
      s.map.get(path.head) match {
        case Some(state) => s.values.map(v => v -> path) ++ state.traverse(path.tail)
        case None        => s.values.map(v => v -> path)
      }

object FSABuilder {

  def update[A](head: State[A], path: String, value: A): State[A] =
    if path.isEmpty then
      //If duplicate path then original value is replaced.
      head.copy(values = head.values :+ value)
    else
      val (key, rest) = (path.head, path.tail)
      val next = head.map.get(key)
      val stateToUpdate = next match {
        case Some(state) => state
        case None        => Empty
      }
      val updated = update(stateToUpdate, rest, value)
      head.copy(map = head.map.updated(key, updated))

  def build[A](list: Seq[(String, A)]) : State[A] =
    list.foldLeft(Empty[A])((state, tuple) => update(state, tuple._1, tuple._2))
}



