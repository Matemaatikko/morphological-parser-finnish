package morph_fin.utils


/***
 * Example: Seq("algorithm", "algotim", "algonat") -> "algo"
 */
object LongestStartingSubstring {
  def apply(list: Seq[String]): String =
    val sortedList = list.sorted
    val first = sortedList.head
    val last = sortedList.last
    var i = 0
    var result = ""
    while(i < first.length && i < last.length && first(i) == last(i))
      result += first(i)
      i += 1
    result
}