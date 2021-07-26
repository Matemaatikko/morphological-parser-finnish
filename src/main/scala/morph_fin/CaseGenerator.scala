package morph_fin


case class ExtensiveRuling(
                            number: Int,
                            drotRightToGetRoot: Int,
                            tailOfLemma: String,
                            cases: Seq[(Case, Type, String)])


object GenerateRuling {

  def apply(ruling: Ruling): ExtensiveRuling =
    val casedWords = ruling.cases.map(_._3)
    val root = findRoot(casedWords)
    val drop = ruling.lemma.length - root.length
    val cases = ruling.cases.map(a => (a._1, a._2, a._3.drop(root.length)))
    ExtensiveRuling(ruling.number, drop, ruling.lemma.drop(root.length), cases)

  def findRoot(list: Seq[String]): String =
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


object GenerateCases {

  def apply(rulings: Seq[ExtensiveRuling], lemma: String, number: Int, gradation: Int) =
    //Gradation
    //Vocalization-type of the word
    //Is Gradation already applied
    //Replacement happens on the place of tail
    ???
}