package morph_fin.fsa


case class State[A](map: Map[Char, State[A]], value: Option[A])
def Empty[A] = State[A](Map(), None)


extension[A] (s: State[A])
  //def print: String =  s.value.toString + "\n" + s.map.toList.map(tuple => tuple._1 + "->" + tuple._2.print).mkString("[","\n", "]")
  def find(path: String): Option[A] =
    if path.isEmpty then s.value
    else
      s.map.get(path.head).flatMap(_.find(path.tail))

  def traverse(path: String): Option[(A, String)] =
    if path.isEmpty then s.value.map(v => v -> path)
    else
      s.map.get(path.head) match {
        case Some(state) => state.traverse(path.tail)
        case None        => s.value.map(v => v -> path)
      }

object FSABuilder {

  def update[A](head: State[A], path: String, value: A): State[A] =
    if path.isEmpty then
      //If duplicate path then original value is replaced.
      head.copy(value = Some(value))
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



