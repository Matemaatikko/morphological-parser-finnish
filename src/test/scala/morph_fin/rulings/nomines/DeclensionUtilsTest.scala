package morph_fin.rulings.nomines

import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.Morphemes

extension[A] (a: A)
  def opt = Some(a)

class DeclensionUtilsTest extends AnyFlatSpec with should.Matchers {

  import morph_fin.rulings.MorphemesUtils._

  import DeclensionUtils._
  val rules = LoadAndParseNomineRules.rules

  extension(list: Seq[ResultWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes == morphemes).map(_.word.toString).toSet == words.toSet)

  "addDeclensions" should "handle case: rule: 1, word: järkky" in {
    val word = Word("järkky", 1, Gradation("kk", "k").opt)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "järkky")
    declensions.matches(Gen:: S, "järkyn")
    declensions.matches(Par:: S, "järkkyä")
    declensions.matches(Acc:: S, "järkky", "järkyn")

    declensions.matches(Ine:: S, "järkyssä")
    declensions.matches(Ela:: S, "järkystä")
    declensions.matches(Ill:: S, "järkkyyn")

    declensions.matches(Ade:: S, "järkyllä")
    declensions.matches(Abl:: S, "järkyltä")
    declensions.matches(All:: S, "järkylle")

    declensions.matches(Ess:: S, "järkkynä")
    declensions.matches(Tra:: S, "järkyksi")
    declensions.matches(Abe:: S, "järkyttä")

    //Plural

    declensions.matches(Nom:: P, "järkyt")
    declensions.matches(Gen:: P, "järkkyjen")
    declensions.matches(Par:: P, "järkkyjä")
    declensions.matches(Acc:: P, "järkyt")

    declensions.matches(Ine:: P, "järkyissä")
    declensions.matches(Ela:: P, "järkyistä")
    declensions.matches(Ill:: P, "järkkyihin")

    declensions.matches(Ade:: P, "järkyillä")
    declensions.matches(Abl:: P, "järkyiltä")
    declensions.matches(All:: P, "järkyille")

    declensions.matches(Ess:: P, "järkkyinä")
    declensions.matches(Tra:: P, "järkyiksi")
    declensions.matches(Abe:: P, "järkyittä")
    declensions.matches(Ins:: P, "järkyin")
    declensions.matches(Com:: P, "järkkyine")
  }

  "addDeclensions" should "handle case: rule: 2, word: jäämistö" in {
    val word = Word("jäämistö", 2, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "jäämistö")
    declensions.matches(Gen:: S, "jäämistön")
    declensions.matches(Par:: S, "jäämistöä")

    declensions.matches(Ill:: S, "jäämistöön")

    declensions.matches(Ade:: S, "jäämistöllä")

    //Plural

    declensions.matches(Nom:: P, "jäämistöt")
    declensions.matches(Gen:: P, "jäämistöjen", "jäämistöiden", "jäämistöitten")
    declensions.matches(Par:: P, "jäämistöjä", "jäämistöitä")

    declensions.matches(Ill:: P, "jäämistöihin")

    declensions.matches(Ade:: P, "jäämistöillä")

    declensions.matches(Ins:: P, "jäämistöin")
    declensions.matches(Com:: P, "jäämistöine")
  }


  "addDeclensions" should "handle case: rule: 3, word: kaavio" in {
    val word = Word("kaavio", 3, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "kaavio")
    declensions.matches(Gen:: S, "kaavion")
    declensions.matches(Par:: S, "kaaviota")

    declensions.matches(Ill:: S, "kaavioon")

    declensions.matches(Ade:: S, "kaaviolla")

    //Plural

    declensions.matches(Nom:: P, "kaaviot")
    declensions.matches(Gen:: P, "kaavioiden", "kaavioitten")
    declensions.matches(Par:: P, "kaavioita")

    declensions.matches(Ill:: P, "kaavioihin")

    declensions.matches(Ade:: P, "kaavioilla")

    declensions.matches(Ins:: P, "kaavioin")
    declensions.matches(Com:: P, "kaavioine")
  }

  "addDeclensions" should "handle case: rule: 4, word: kaislikko" in {
    val word = Word("kaislikko", 4, Gradation("kk", "k").opt)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "kaislikko")
    declensions.matches(Gen:: S, "kaislikon")
    declensions.matches(Par:: S, "kaislikkoa")

    declensions.matches(Ill:: S, "kaislikkoon")

    declensions.matches(Ade:: S, "kaislikolla")

    //Plural

    declensions.matches(Nom:: P, "kaislikot")
    declensions.matches(Gen:: P, "kaislikoiden", "kaislikoitten", "kaislikkojen")
    declensions.matches(Par:: P, "kaislikoita", "kaislikkoja")

    declensions.matches(Ill:: P, "kaislikkoihin", "kaislikoihin")

    declensions.matches(Ade:: P, "kaislikoilla")

    declensions.matches(Ins:: P, "kaislikoin")
    declensions.matches(Com:: P, "kaislikoine")
  }

  "addDeclensions" should "handle case: rule: 5, word: kalium" in {
    val word = Word("kalium", 5, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "kalium")
    declensions.matches(Gen:: S, "kaliumin")
    declensions.matches(Par:: S, "kaliumia")

    declensions.matches(Ill:: S, "kaliumiin")

    declensions.matches(Ade:: S, "kaliumilla")

    //Plural

    declensions.matches(Nom:: P, "kaliumit")
    declensions.matches(Gen:: P, "kaliumien")
    declensions.matches(Par:: P, "kaliumeja")

    declensions.matches(Ill:: P, "kaliumeihin")

    declensions.matches(Ade:: P, "kaliumeilla")

    declensions.matches(Ins:: P, "kaliumein")
    declensions.matches(Com:: P, "kaliumeine")
  }

  "addDeclensions" should "handle case: rule: 5, word: kalkki" in {
    val word = Word("kalkki", 5, Gradation("kk", "k").opt)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "kalkki")
    declensions.matches(Gen:: S, "kalkin")
    declensions.matches(Par:: S, "kalkkia")

    declensions.matches(Ill:: S, "kalkkiin")

    declensions.matches(Ade:: S, "kalkilla")

    //Plural

    declensions.matches(Nom:: P, "kalkit")
    declensions.matches(Gen:: P, "kalkkien")
    declensions.matches(Par:: P, "kalkkeja")

    declensions.matches(Ill:: P, "kalkkeihin")

    declensions.matches(Ade:: P, "kalkeilla")

    declensions.matches(Ins:: P, "kalkein")
    declensions.matches(Com:: P, "kalkkeine")
  }

  "addDeclensions" should "handle case: rule: 6, word: kalori" in {
    val word = Word("kalori", 6, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "kalori")
    declensions.matches(Gen:: S, "kalorin")
    declensions.matches(Par:: S, "kaloria")

    declensions.matches(Ill:: S, "kaloriin")

    declensions.matches(Ade:: S, "kalorilla")

    //Plural

    declensions.matches(Nom:: P, "kalorit")
    declensions.matches(Gen:: P, "kalorien", "kaloreiden", "kaloreitten")
    declensions.matches(Par:: P, "kaloreja", "kaloreita")

    declensions.matches(Ill:: P, "kaloreihin")

    declensions.matches(Ade:: P, "kaloreilla")

    declensions.matches(Ins:: P, "kalorein")
    declensions.matches(Com:: P, "kaloreine")
  }

  "addDeclensions" should "handle case: rule: 6, word: kalterit (plural parsing)" in {
    val word = Word("kalterit", 6, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "kalteri")
    declensions.matches(Gen:: S, "kalterin")
    declensions.matches(Par:: S, "kalteria")

    declensions.matches(Ill:: S, "kalteriin")

    declensions.matches(Ade:: S, "kalterilla")

    //Plural

    declensions.matches(Nom:: P, "kalterit")
    declensions.matches(Gen:: P, "kalterien", "kaltereiden", "kaltereitten")
    declensions.matches(Par:: P, "kaltereita", "kaltereja")

    declensions.matches(Ill:: P, "kaltereihin")

    declensions.matches(Ade:: P, "kaltereilla")

    declensions.matches(Ins:: P, "kalterein")
    declensions.matches(Com:: P, "kaltereine")
  }

  "addDeclensions" should "handle case: rule: 7, word: kanki" in {
    val word = Word("kanki", 7, Gradation("nk", "ng").opt)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "kanki")
    declensions.matches(Gen:: S, "kangen")
    declensions.matches(Par:: S, "kankea")

    declensions.matches(Ill:: S, "kankeen")

    declensions.matches(Ade:: S, "kangella")

    //Plural

    declensions.matches(Nom:: P, "kanget")
    declensions.matches(Gen:: P, "kankien")
    declensions.matches(Par:: P, "kankia")

    declensions.matches(Ill:: P, "kankiin")

    declensions.matches(Ade:: P, "kangilla")

    declensions.matches(Ins:: P, "kangin")
    declensions.matches(Com:: P, "kankine")
  }

  "addDeclensions" should "handle case: rule: 8, word: manne" in {
    val word = Word("manne", 8, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "manne")
    declensions.matches(Gen:: S, "mannen")
    declensions.matches(Par:: S, "mannea")

    declensions.matches(Ill:: S, "manneen")

    declensions.matches(Ade:: S, "mannella")

    //Plural

    declensions.matches(Nom:: P, "mannet")
    declensions.matches(Gen:: P, "mannejen", "mannein")
    declensions.matches(Par:: P, "manneja")

    declensions.matches(Ill:: P, "manneihin")

    declensions.matches(Ade:: P, "manneilla")

    declensions.matches(Ins:: P, "mannein")
    declensions.matches(Com:: P, "manneine")
  }

  "addDeclensions" should "handle case: rule: 9, word: marja-aika" in {
    val word = Word("marja-aika", 9, Gradation("k", "").opt)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "marja-aika")
    declensions.matches(Gen:: S, "marja-ajan")
    declensions.matches(Par:: S, "marja-aikaa")

    declensions.matches(Ill:: S, "marja-aikaan")

    declensions.matches(Ade:: S, "marja-ajalla")

    //Plural

    declensions.matches(Nom:: P, "marja-ajat")
    declensions.matches(Gen:: P, "marja-aikojen", "marja-aikain")
    declensions.matches(Par:: P, "marja-aikoja")

    declensions.matches(Ill:: P, "marja-aikoihin")

    declensions.matches(Ade:: P, "marja-ajoilla")

    declensions.matches(Ins:: P, "marja-ajoin")
    declensions.matches(Com:: P, "marja-aikoine")
  }

  "addDeclensions" should "handle case: rule: 10, word: matala" in {
    val word = Word("matala", 10, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "matala")
    declensions.matches(Gen:: S, "matalan")
    declensions.matches(Par:: S, "matalaa")

    declensions.matches(Ill:: S, "matalaan")

    declensions.matches(Ade:: S, "matalalla")

    //Plural

    declensions.matches(Nom:: P, "matalat")
    declensions.matches(Gen:: P, "matalien", "matalain")
    declensions.matches(Par:: P, "matalia")

    declensions.matches(Ill:: P, "mataliin")

    declensions.matches(Ade:: P, "matalilla")

    declensions.matches(Ins:: P, "matalin")
    declensions.matches(Com:: P, "mataline")
  }

  "addDeclensions" should "handle case: rule: 11, word: papana" in {
    val word = Word("papana", 11, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "papana")
    declensions.matches(Gen:: S, "papanan")
    declensions.matches(Par:: S, "papanaa")

    declensions.matches(Ill:: S, "papanaan")

    declensions.matches(Ade:: S, "papanalla")

    //Plural

    declensions.matches(Nom:: P, "papanat")
    declensions.matches(Gen:: P, "papanien", "papanoiden", "papanoitten", "papanojen", "papanain")
    declensions.matches(Par:: P, "papania", "papanoita", "papanoja")

    declensions.matches(Ill:: P, "papanoihin", "papaniin")

    declensions.matches(Ade:: P, "papanilla", "papanoilla")

    declensions.matches(Ins:: P, "papanin", "papanoin")
    declensions.matches(Com:: P, "papanine")
  }

  "addDeclensions" should "handle case: rule: 12, word: passiivat = velat (plural)" in {
    val word = Word("passiivat", 12, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "passiiva")
    declensions.matches(Gen:: S, "passiivan")
    declensions.matches(Par:: S, "passiivaa")

    declensions.matches(Ill:: S, "passiivaan")

    declensions.matches(Ade:: S, "passiivalla")

    //Plural

    declensions.matches(Nom:: P, "passiivat")
    declensions.matches(Gen:: P, "passiivoiden", "passiivoitten", "passiivain")
    declensions.matches(Par:: P, "passiivoita")

    declensions.matches(Ill:: P, "passiivoihin")

    declensions.matches(Ade:: P, "passiivoilla")

    declensions.matches(Ins:: P, "passiivoin")
    declensions.matches(Com:: P, "passiivoine")
  }

  "addDeclensions" should "handle case: rule: 12, word: pasteija (Singular)" in {
    val word = Word("pasteija", 12, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "pasteija")
    declensions.matches(Gen:: S, "pasteijan")
    declensions.matches(Par:: S, "pasteijaa")

    declensions.matches(Ill:: S, "pasteijaan")

    declensions.matches(Ade:: S, "pasteijalla")

    //Plural

    declensions.matches(Nom:: P, "pasteijat")
    declensions.matches(Gen:: P, "pasteijoiden", "pasteijoitten", "pasteijain")
    declensions.matches(Par:: P, "pasteijoita")

    declensions.matches(Ill:: P, "pasteijoihin")

    declensions.matches(Ade:: P, "pasteijoilla")

    declensions.matches(Ins:: P, "pasteijoin")
    declensions.matches(Com:: P, "pasteijoine")
  }

  "addDeclensions" should "handle case: rule: 13, word: perusta" in {
    val word = Word("perusta", 13, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "perusta")
    declensions.matches(Gen:: S, "perustan")
    declensions.matches(Par:: S, "perustaa")

    declensions.matches(Ill:: S, "perustaan")

    declensions.matches(Ade:: S, "perustalla")

    //Plural

    declensions.matches(Nom:: P, "perustat")
    declensions.matches(Gen:: P, "perustojen", "perustoiden", "perustoitten", "perustain")
    declensions.matches(Par:: P, "perustoja", "perustoita")

    declensions.matches(Ill:: P, "perustoihin")

    declensions.matches(Ade:: P, "perustoilla")

    declensions.matches(Ins:: P, "perustoin")
    declensions.matches(Com:: P, "perustoine")
  }

  "addDeclensions" should "handle case: rule: 14, word: pohatta" in {
    val word = Word("pohatta", 14, Gradation("tt", "t").opt)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "pohatta")
    declensions.matches(Gen:: S, "pohatan")
    declensions.matches(Par:: S, "pohattaa")

    declensions.matches(Ill:: S, "pohattaan")

    declensions.matches(Ade:: S, "pohatalla")

    //Plural

    declensions.matches(Nom:: P, "pohatat")
    declensions.matches(Gen:: P, "pohattojen", "pohatoiden", "pohatoitten", "pohattain")
    declensions.matches(Par:: P, "pohattoja", "pohatoita")

    declensions.matches(Ill:: P, "pohattoihin", "pohatoihin")

    declensions.matches(Ade:: P, "pohatoilla")

    declensions.matches(Ess:: P, "pohatoina", "pohattoina")

    declensions.matches(Ins:: P, "pohatoin")
    declensions.matches(Com:: P, "pohatoine", "pohattoine")
  }

}
