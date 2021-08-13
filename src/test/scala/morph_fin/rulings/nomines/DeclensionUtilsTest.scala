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
      assert(list.filter(_.morphemes == morphemes).map(_.word.toString).toSet == words.toSet, morphemes)

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

  "addDeclensions" should "handle case: rule: 15, word: hilpeä" in {
    val word = Word("hilpeä", 15, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "hilpeä")
    declensions.matches(Gen:: S, "hilpeän")
    declensions.matches(Par:: S, "hilpeää")

    declensions.matches(Ill:: S, "hilpeään")

    declensions.matches(Ade:: S, "hilpeällä")

    //Plural

    declensions.matches(Nom:: P, "hilpeät")
    declensions.matches(Gen:: P, "hilpeiden", "hilpeitten", "hilpeäin")
    declensions.matches(Par:: P, "hilpeitä")

    declensions.matches(Ill:: P, "hilpeihin", "hilpeisiin")

    declensions.matches(Ade:: P, "hilpeillä")

    declensions.matches(Ess:: P, "hilpeinä")

    declensions.matches(Ins:: P, "hilpein")
    declensions.matches(Com:: P, "hilpeine")
  }

  "addDeclensions" should "handle case: rule: 16, word: likempi" in {
    val word = Word("likempi", 16, Gradation("mp", "mm").opt)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "likempi")
    declensions.matches(Gen:: S, "likemmän")
    declensions.matches(Par:: S, "likempää")

    declensions.matches(Ill:: S, "likempään")

    declensions.matches(Ade:: S, "likemmällä")

    //Plural

    declensions.matches(Nom:: P, "likemmät")
    declensions.matches(Gen:: P, "likempien", "likempäin")
    declensions.matches(Par:: P, "likempiä")

    declensions.matches(Ill:: P, "likempiin")

    declensions.matches(Ade:: P, "likemmillä")

    declensions.matches(Ess:: P, "likempinä")

    declensions.matches(Ins:: P, "likemmin")
    declensions.matches(Com:: P, "likempine")
  }

  "addDeclensions" should "handle case: rule: 17, word: palttoo" in {
    val word = Word("palttoo", 17, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "palttoo")
    declensions.matches(Gen:: S, "palttoon")
    declensions.matches(Par:: S, "palttoota")

    declensions.matches(Ill:: S, "palttooseen")

    declensions.matches(Ade:: S, "palttoolla")

    //Plural

    declensions.matches(Nom:: P, "palttoot")
    declensions.matches(Gen:: P, "palttoiden", "palttoitten")
    declensions.matches(Par:: P, "palttoita")

    declensions.matches(Ill:: P, "palttoisiin", "palttoihin")

    declensions.matches(Ade:: P, "palttoilla")

    declensions.matches(Ess:: P, "palttoina")

    declensions.matches(Ins:: P, "palttoin")
    declensions.matches(Com:: P, "palttoine")
  }

  "addDeclensions" should "handle case: rule: 18, word: perjantai" in {
    val word = Word("perjantai", 18, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "perjantai")
    declensions.matches(Gen:: S, "perjantain")
    declensions.matches(Par:: S, "perjantaita")

    declensions.matches(Ill:: S, "perjantaihin")

    declensions.matches(Ade:: S, "perjantailla")

    //Plural

    declensions.matches(Nom:: P, "perjantait")
    declensions.matches(Gen:: P, "perjantaiden", "perjantaitten")
    declensions.matches(Par:: P, "perjantaita")

    declensions.matches(Ill:: P, "perjantaihin")

    declensions.matches(Ade:: P, "perjantailla")

    declensions.matches(Ess:: P, "perjantaina")

    declensions.matches(Ins:: P, "perjantain")
    declensions.matches(Com:: P, "perjantaine")
  }

  "addDeclensions" should "handle case: rule: 19, word: tie" in {
    val word = Word("tie", 19, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "tie")
    declensions.matches(Gen:: S, "tien")
    declensions.matches(Par:: S, "tietä")

    declensions.matches(Ill:: S, "tiehen")

    declensions.matches(Ade:: S, "tiellä")

    //Plural

    declensions.matches(Nom:: P, "tiet")
    declensions.matches(Gen:: P, "teiden", "teitten")
    declensions.matches(Par:: P, "teitä")

    declensions.matches(Ill:: P, "teihin")

    declensions.matches(Ade:: P, "teillä")

    declensions.matches(Ess:: P, "teinä")

    declensions.matches(Ins:: P, "tein")
    declensions.matches(Com:: P, "teine")
  }

  "addDeclensions" should "handle case: rule: 21, word: coupé" in {
    val word = Word("coupé", 21, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "coupé")
    declensions.matches(Gen:: S, "coupén")
    declensions.matches(Par:: S, "coupéta")

    declensions.matches(Ill:: S, "coupéhen")

    declensions.matches(Ade:: S, "coupélla")

    //Plural

    declensions.matches(Nom:: P, "coupét")
    declensions.matches(Gen:: P, "coupéiden", "coupéitten")
    declensions.matches(Par:: P, "coupéita")

    declensions.matches(Ill:: P, "coupéihin")

    declensions.matches(Ade:: P, "coupéilla")

    declensions.matches(Ess:: P, "coupéina")

    declensions.matches(Ins:: P, "coupéin")
    declensions.matches(Com:: P, "coupéine")
  }

  "addDeclensions" should "handle case: rule: 22, word: nougat" in {
    val word = Word("nougat", 22, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "nougat")
    declensions.matches(Gen:: S, "nougat'n")
    declensions.matches(Par:: S, "nougat'ta")

    declensions.matches(Ill:: S, "nougat'hen")

    declensions.matches(Ade:: S, "nougat'lla")

    //Plural

    declensions.matches(Nom:: P, "nougat't")
    declensions.matches(Gen:: P, "nougat'iden", "nougat'itten")
    declensions.matches(Par:: P, "nougat'ita")

    declensions.matches(Ill:: P, "nougat'ihin")

    declensions.matches(Ade:: P, "nougat'illa")

    declensions.matches(Ess:: P, "nougat'ina")

    declensions.matches(Ins:: P, "nougat'in")
    declensions.matches(Com:: P, "nougat'ine")
  }

  "addDeclensions" should "handle case: rule: 23, word: tuli" in {
    val word = Word("tuli", 23, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "tuli")
    declensions.matches(Gen:: S, "tulen")
    declensions.matches(Par:: S, "tulta")

    declensions.matches(Ill:: S, "tuleen")

    declensions.matches(Ade:: S, "tulella")

    //Plural

    declensions.matches(Nom:: P, "tulet")
    declensions.matches(Gen:: P, "tulien")
    declensions.matches(Par:: P, "tulia")

    declensions.matches(Ill:: P, "tuliin")

    declensions.matches(Ade:: P, "tulilla")

    declensions.matches(Ess:: P, "tulina")

    declensions.matches(Ins:: P, "tulin")
    declensions.matches(Com:: P, "tuline")
  }

  "addDeclensions" should "handle case: rule: 24, word: hiili" in {
    val word = Word("hiili", 24, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "hiili")
    declensions.matches(Gen:: S, "hiilen")
    declensions.matches(Par:: S, "hiiltä")

    declensions.matches(Ill:: S, "hiileen")

    declensions.matches(Ade:: S, "hiilellä")

    //Plural

    declensions.matches(Nom:: P, "hiilet")
    declensions.matches(Gen:: P, "hiilien", "hiilten")
    declensions.matches(Par:: P, "hiiliä")

    declensions.matches(Ill:: P, "hiiliin")

    declensions.matches(Ade:: P, "hiilillä")

    declensions.matches(Ess:: P, "hiilinä")

    declensions.matches(Ins:: P, "hiilin")
    declensions.matches(Com:: P, "hiiline")
  }

  "addDeclensions" should "handle case: rule: 25, word: lumi" in {
    val word = Word("lumi", 25, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "lumi")
    declensions.matches(Gen:: S, "lumen")
    declensions.matches(Par:: S, "lunta", "lumea")

    declensions.matches(Ill:: S, "lumeen")

    declensions.matches(Ade:: S, "lumella")

    //Plural

    declensions.matches(Nom:: P, "lumet")
    declensions.matches(Gen:: P, "lumien", "lunten")
    declensions.matches(Par:: P, "lumia")

    declensions.matches(Ill:: P, "lumiin")

    declensions.matches(Ade:: P, "lumilla")

    declensions.matches(Ess:: P, "lumina")

    declensions.matches(Ins:: P, "lumin")
    declensions.matches(Com:: P, "lumine")
  }

  "addDeclensions" should "handle case: rule: 26, word: mieli" in {
    val word = Word("mieli", 26, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "mieli")
    declensions.matches(Gen:: S, "mielen")
    declensions.matches(Par:: S, "mieltä")

    declensions.matches(Ill:: S, "mieleen")

    declensions.matches(Ade:: S, "mielellä")

    //Plural

    declensions.matches(Nom:: P, "mielet")
    declensions.matches(Gen:: P, "mielten", "mielien")
    declensions.matches(Par:: P, "mieliä")

    declensions.matches(Ill:: P, "mieliin")

    declensions.matches(Ade:: P, "mielillä")

    declensions.matches(Ess:: P, "mielinä")

    declensions.matches(Ins:: P, "mielin")
    declensions.matches(Com:: P, "mieline")
  }

  "addDeclensions" should "handle case: rule: 27, word: reisi" in {
    val word = Word("reisi", 27, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "reisi")
    declensions.matches(Gen:: S, "reiden")
    declensions.matches(Par:: S, "reittä")

    declensions.matches(Ill:: S, "reiteen")

    declensions.matches(Ade:: S, "reidellä")

    //Plural

    declensions.matches(Nom:: P, "reidet")
    declensions.matches(Gen:: P, "reisien", "reitten")
    declensions.matches(Par:: P, "reisiä")

    declensions.matches(Ill:: P, "reisiin")

    declensions.matches(Ade:: P, "reisillä")

    declensions.matches(Ess:: P, "reisinä")

    declensions.matches(Ins:: P, "reisin")
    declensions.matches(Com:: P, "reisine")
  }

  "addDeclensions" should "handle case: rule: 28, word: virsi" in {
    val word = Word("virsi", 28, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "virsi")
    declensions.matches(Gen:: S, "virren")
    declensions.matches(Par:: S, "virttä")

    declensions.matches(Ill:: S, "virteen")

    declensions.matches(Ade:: S, "virrellä")

    //Plural

    declensions.matches(Nom:: P, "virret")
    declensions.matches(Gen:: P, "virsien", "virtten")
    declensions.matches(Par:: P, "virsiä")

    declensions.matches(Ill:: P, "virsiin")

    declensions.matches(Ade:: P, "virsillä")

    declensions.matches(Ess:: P, "virsinä")

    declensions.matches(Ins:: P, "virsin")
    declensions.matches(Com:: P, "virsine")
  }

  "addDeclensions" should "handle case: rule: 29, word: lapsi" in {
    val word = Word("lapsi", 29, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "lapsi")
    declensions.matches(Gen:: S, "lapsen")
    declensions.matches(Par:: S, "lasta")

    declensions.matches(Ill:: S, "lapseen")

    declensions.matches(Ade:: S, "lapsella")

    //Plural

    declensions.matches(Nom:: P, "lapset")
    declensions.matches(Gen:: P, "lapsien", "lasten")
    declensions.matches(Par:: P, "lapsia")

    declensions.matches(Ill:: P, "lapsiin")

    declensions.matches(Ade:: P, "lapsilla")

    declensions.matches(Ess:: P, "lapsina")

    declensions.matches(Ins:: P, "lapsin")
    declensions.matches(Com:: P, "lapsine")
  }

  "addDeclensions" should "handle case: rule: 30, word: peitsi" in {
    val word = Word("peitsi", 30, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "peitsi")
    declensions.matches(Gen:: S, "peitsen")
    declensions.matches(Par:: S, "peistä")

    declensions.matches(Ill:: S, "peitseen")

    declensions.matches(Ade:: S, "peitsellä")

    //Plural

    declensions.matches(Nom:: P, "peitset")
    declensions.matches(Gen:: P, "peitsien", "peisten")
    declensions.matches(Par:: P, "peitsiä")

    declensions.matches(Ill:: P, "peitsiin")

    declensions.matches(Ade:: P, "peitsillä")

    declensions.matches(Ess:: P, "peitsinä")

    declensions.matches(Ins:: P, "peitsin")
    declensions.matches(Com:: P, "peitsine")
  }

  "addDeclensions" should "handle case: rule: 31, word: yksi" in {
    val word = Word("yksi", 31, None)
    val declensions = addDeclesions(rules, word)

    //Singular

    declensions.matches(Nom:: S, "yksi")
    declensions.matches(Gen:: S, "yhden")
    declensions.matches(Par:: S, "yhtä")

    declensions.matches(Ill:: S, "yhteen")

    declensions.matches(Ade:: S, "yhdellä")

    //Plural

    declensions.matches(Nom:: P, "yhdet")
    declensions.matches(Gen:: P, "yksien")
    declensions.matches(Par:: P, "yksiä")

    declensions.matches(Ill:: P, "yksiin")

    declensions.matches(Ade:: P, "yksillä")

    declensions.matches(Ess:: P, "yksinä")

    declensions.matches(Ins:: P, "yksin")
    declensions.matches(Com:: P, "yksine")
  }

}
