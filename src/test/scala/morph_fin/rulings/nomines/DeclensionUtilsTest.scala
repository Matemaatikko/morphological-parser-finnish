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
  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules

  extension(list: Seq[ResultWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes == morphemes).map(_.word.toString).toSet == words.toSet, morphemes)

  "addDeclensions" should "handle case: rule: 1, word: järkky" in {
    val word = Word("järkky", 1, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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
    val declensions = generateDeclensions(word)

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

  "addDeclensions" should "handle case: rule: 32, word: aamen" in {
    val word = Word("aamen", 32, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "aamen")
    declensions.matches(Gen:: S, "aamenen")
    declensions.matches(Par:: S, "aamenta")

    declensions.matches(Ill:: S, "aameneen")

    declensions.matches(Ade:: S, "aamenella")

    //Plural

    declensions.matches(Nom:: P, "aamenet")
    declensions.matches(Gen:: P, "aamenien", "aamenten")
    declensions.matches(Par:: P, "aamenia")

    declensions.matches(Ill:: P, "aameniin")

    declensions.matches(Ade:: P, "aamenilla")

    declensions.matches(Ess:: P, "aamenina")

    declensions.matches(Ins:: P, "aamenin")
    declensions.matches(Com:: P, "aamenine")
  }

  "addDeclensions" should "handle case: rule: 33, word: ahdin" in {
    val word = Word("ahdin", 33, Gradation("t", "d").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "ahdin")
    declensions.matches(Gen:: S, "ahtimen")
    declensions.matches(Par:: S, "ahdinta")

    declensions.matches(Ill:: S, "ahtimeen")

    declensions.matches(Ade:: S, "ahtimella")

    //Plural

    declensions.matches(Nom:: P, "ahtimet")
    declensions.matches(Gen:: P, "ahdinten", "ahtimien")
    declensions.matches(Par:: P, "ahtimia")

    declensions.matches(Ill:: P, "ahtimiin")

    declensions.matches(Ade:: P, "ahtimilla")

    declensions.matches(Ess:: P, "ahtimina")

    declensions.matches(Ins:: P, "ahtimin")
    declensions.matches(Com:: P, "ahtimine")
  }

  "addDeclensions" should "handle case: rule: 34, word: aineeton" in {
    val word = Word("aineeton", 34, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "aineeton")
    declensions.matches(Gen:: S, "aineettoman")
    declensions.matches(Par:: S, "aineetonta")

    declensions.matches(Ill:: S, "aineettomaan")

    declensions.matches(Ade:: S, "aineettomalla")

    //Plural

    declensions.matches(Nom:: P, "aineettomat")
    declensions.matches(Gen:: P, "aineetonten", "aineettomien")
    declensions.matches(Par:: P, "aineettomia")

    declensions.matches(Ill:: P, "aineettomiin")

    declensions.matches(Ade:: P, "aineettomilla")

    declensions.matches(Ess:: P, "aineettomina")

    declensions.matches(Ins:: P, "aineettomin")
    declensions.matches(Com:: P, "aineettomine")
  }

  "addDeclensions" should "handle case: rule: 35, word: lämmin" in {
    val word = Word("lämmin", 35, Gradation("mp", "mm").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "lämmin")
    declensions.matches(Gen:: S, "lämpimän")
    declensions.matches(Par:: S, "lämmintä")

    declensions.matches(Ill:: S, "lämpimään")

    declensions.matches(Ade:: S, "lämpimällä")

    //Plural

    declensions.matches(Nom:: P, "lämpimät")
    declensions.matches(Gen:: P, "lämpimien", "lämpimäin")
    declensions.matches(Par:: P, "lämpimiä")

    declensions.matches(Ill:: P, "lämpimiin")

    declensions.matches(Ade:: P, "lämpimillä")

    declensions.matches(Ess:: P, "lämpiminä")

    declensions.matches(Ins:: P, "lämpimin")
    declensions.matches(Com:: P, "lämpimine")
  }

  "addDeclensions" should "handle case: rule: 36, word: parhain" in {
    val word = Word("parhain", 36, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "parhain")
    declensions.matches(Gen:: S, "parhaimman")
    declensions.matches(Par:: S, "parhainta")

    declensions.matches(Ill:: S, "parhaimpaan")

    declensions.matches(Ade:: S, "parhaimmalla")

    //Plural

    declensions.matches(Nom:: P, "parhaimmat")
    declensions.matches(Gen:: P, "parhaimpien", "parhainten", "parhaimpain")
    declensions.matches(Par:: P, "parhaimpia")

    declensions.matches(Ill:: P, "parhaimpiin")

    declensions.matches(Ade:: P, "parhaimmilla")

    declensions.matches(Ess:: P, "parhaimpina")

    declensions.matches(Ins:: P, "parhaimmin")
    declensions.matches(Com:: P, "parhaimpine")
  }

  "addDeclensions" should "handle case: rule: 37, word: vasen" in {
    val word = Word("vasen", 37, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vasen")
    declensions.matches(Gen:: S, "vasemman")
    declensions.matches(Par:: S, "vasenta", "vasempaa")

    declensions.matches(Ill:: S, "vasempaan")

    declensions.matches(Ade:: S, "vasemmalla")

    //Plural

    declensions.matches(Nom:: P, "vasemmat")
    declensions.matches(Gen:: P, "vasempien", "vasenten", "vasempain")
    declensions.matches(Par:: P, "vasempia")

    declensions.matches(Ill:: P, "vasempiin")

    declensions.matches(Ade:: P, "vasemmilla")

    declensions.matches(Ess:: P, "vasempina")

    declensions.matches(Ins:: P, "vasemmin")
    declensions.matches(Com:: P, "vasempine")
  }

  "addDeclensions" should "handle case: rule: 38, word: vastuullinen" in {
    val word = Word("vastuullinen", 38, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vastuullinen")
    declensions.matches(Gen:: S, "vastuullisen")
    declensions.matches(Par:: S, "vastuullista")

    declensions.matches(Ill:: S, "vastuulliseen")

    declensions.matches(Ade:: S, "vastuullisella")

    //Plural

    declensions.matches(Nom:: P, "vastuulliset")
    declensions.matches(Gen:: P, "vastuullisten", "vastuullisien")
    declensions.matches(Par:: P, "vastuullisia")

    declensions.matches(Ill:: P, "vastuullisiin")

    declensions.matches(Ade:: P, "vastuullisilla")

    declensions.matches(Ess:: P, "vastuullisina")

    declensions.matches(Ins:: P, "vastuullisin")
    declensions.matches(Com:: P, "vastuullisine")
  }

  "addDeclensions" should "handle case: rule: 39, word: vavahdus" in {
    val word = Word("vavahdus", 39, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vavahdus")
    declensions.matches(Gen:: S, "vavahduksen")
    declensions.matches(Par:: S, "vavahdusta")

    declensions.matches(Ill:: S, "vavahdukseen")

    declensions.matches(Ade:: S, "vavahduksella")

    //Plural

    declensions.matches(Nom:: P, "vavahdukset")
    declensions.matches(Gen:: P, "vavahdusten", "vavahduksien")
    declensions.matches(Par:: P, "vavahduksia")

    declensions.matches(Ill:: P, "vavahduksiin")

    declensions.matches(Ade:: P, "vavahduksilla")

    declensions.matches(Ess:: P, "vavahduksina")

    declensions.matches(Ins:: P, "vavahduksin")
    declensions.matches(Com:: P, "vavahduksine")
  }

  "addDeclensions" should "handle case: rule: 40, word: velkaisuus" in {
    val word = Word("velkaisuus", 40, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "velkaisuus")
    declensions.matches(Gen:: S, "velkaisuuden")
    declensions.matches(Par:: S, "velkaisuutta")

    declensions.matches(Ill:: S, "velkaisuuteen")

    declensions.matches(Ade:: S, "velkaisuudella")

    //Plural

    declensions.matches(Nom:: P, "velkaisuudet")
    declensions.matches(Gen:: P, "velkaisuuksien")
    declensions.matches(Par:: P, "velkaisuuksia")

    declensions.matches(Ill:: P, "velkaisuuksiin")

    declensions.matches(Ade:: P, "velkaisuuksilla")

    declensions.matches(Ess:: P, "velkaisuuksina")

    declensions.matches(Ins:: P, "velkaisuuksin")
    declensions.matches(Com:: P, "velkaisuuksine")
  }

  "addDeclensions" should "handle case: rule: 41, word: viekas" in {
    val word = Word("viekas", 41, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "viekas")
    declensions.matches(Gen:: S, "viekkaan")
    declensions.matches(Par:: S, "viekasta")

    declensions.matches(Ill:: S, "viekkaaseen")

    declensions.matches(Ade:: S, "viekkaalla")

    //Plural

    declensions.matches(Nom:: P, "viekkaat")
    declensions.matches(Gen:: P, "viekkaiden", "viekkaitten")
    declensions.matches(Par:: P, "viekkaita")

    declensions.matches(Ill:: P, "viekkaisiin", "viekkaihin")

    declensions.matches(Ade:: P, "viekkailla")

    declensions.matches(Ess:: P, "viekkaina")

    declensions.matches(Ins:: P, "viekkain")
    declensions.matches(Com:: P, "viekkaine")
  }

  "addDeclensions" should "handle case: rule: 42, word: mies" in {
    val word = Word("mies", 42, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "mies")
    declensions.matches(Gen:: S, "miehen")
    declensions.matches(Par:: S, "miestä")

    declensions.matches(Ill:: S, "mieheen")

    declensions.matches(Ade:: S, "miehellä")

    //Plural

    declensions.matches(Nom:: P, "miehet")
    declensions.matches(Gen:: P, "miesten", "miehien")
    declensions.matches(Par:: P, "miehiä")

    declensions.matches(Ill:: P, "miehiin")

    declensions.matches(Ade:: P, "miehillä")

    declensions.matches(Ess:: P, "miehinä")

    declensions.matches(Ins:: P, "miehin")
    declensions.matches(Com:: P, "miehine")
  }

  "addDeclensions" should "handle case: rule: 43, word: kevyt" in {
    val word = Word("kevyt", 43, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kevyt")
    declensions.matches(Gen:: S, "kevyen")
    declensions.matches(Par:: S, "kevyttä")

    declensions.matches(Ill:: S, "kevyeen")

    declensions.matches(Ade:: S, "kevyellä")

    //Plural

    declensions.matches(Nom:: P, "kevyet")
    declensions.matches(Gen:: P, "kevyiden", "kevyitten")
    declensions.matches(Par:: P, "kevyitä")

    declensions.matches(Ill:: P, "kevyihin", "kevyisiin")

    declensions.matches(Ade:: P, "kevyillä")

    declensions.matches(Ess:: P, "kevyinä")

    declensions.matches(Ins:: P, "kevyin")
    declensions.matches(Com:: P, "kevyine")
  }

  "addDeclensions" should "handle case: rule: 44, word: kevät" in {
    val word = Word("kevät", 44, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kevät")
    declensions.matches(Gen:: S, "kevään")
    declensions.matches(Par:: S, "kevättä")

    declensions.matches(Ill:: S, "kevääseen")

    declensions.matches(Ade:: S, "keväällä")

    //Plural

    declensions.matches(Nom:: P, "keväät")
    declensions.matches(Gen:: P, "keväiden", "keväitten")
    declensions.matches(Par:: P, "keväitä")

    declensions.matches(Ill:: P, "keväisiin", "keväihin")

    declensions.matches(Ade:: P, "keväillä")

    declensions.matches(Ess:: P, "keväinä")

    declensions.matches(Ins:: P, "keväin")
    declensions.matches(Com:: P, "keväine")
  }

  "addDeclensions" should "handle case: rule: 45, word: kuudes" in {
    val word = Word("kuudes", 45, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kuudes")
    declensions.matches(Gen:: S, "kuudennen")
    declensions.matches(Par:: S, "kuudetta")

    declensions.matches(Ill:: S, "kuudenteen")

    declensions.matches(Ade:: S, "kuudennella")

    //Plural

    declensions.matches(Nom:: P, "kuudennet")
    declensions.matches(Gen:: P, "kuudensien")
    declensions.matches(Par:: P, "kuudensia")

    declensions.matches(Ill:: P, "kuudensiin")

    declensions.matches(Ade:: P, "kuudensilla")

    declensions.matches(Ess:: P, "kuudensina")

    declensions.matches(Ins:: P, "kuudensin")
    declensions.matches(Com:: P, "kuudensine")
  }

  "addDeclensions" should "handle case: rule: 46, word: tuhat" in {
    val word = Word("tuhat", 46, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "tuhat")
    declensions.matches(Gen:: S, "tuhannen")
    declensions.matches(Par:: S, "tuhatta")

    declensions.matches(Ill:: S, "tuhanteen")

    declensions.matches(Ade:: S, "tuhannella")

    //Plural

    declensions.matches(Nom:: P, "tuhannet")
    declensions.matches(Gen:: P, "tuhansien", "tuhanten")
    declensions.matches(Par:: P, "tuhansia")

    declensions.matches(Ill:: P, "tuhansiin")

    declensions.matches(Ade:: P, "tuhansilla")

    declensions.matches(Ess:: P, "tuhansina")

    declensions.matches(Ins:: P, "tuhansin")
    declensions.matches(Com:: P, "tuhansine")
  }

  "addDeclensions" should "handle case: rule: 47, word: uudestisyntynyt" in {
    val word = Word("uudestisyntynyt", 47, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "uudestisyntynyt")
    declensions.matches(Gen:: S, "uudestisyntyneen")
    declensions.matches(Par:: S, "uudestisyntynyttä")

    declensions.matches(Ill:: S, "uudestisyntyneeseen")

    declensions.matches(Ade:: S, "uudestisyntyneellä")

    //Plural

    declensions.matches(Nom:: P, "uudestisyntyneet")
    declensions.matches(Gen:: P, "uudestisyntyneiden", "uudestisyntyneitten")
    declensions.matches(Par:: P, "uudestisyntyneitä")

    declensions.matches(Ill:: P, "uudestisyntyneisiin", "uudestisyntyneihin")

    declensions.matches(Ade:: P, "uudestisyntyneillä")

    declensions.matches(Ess:: P, "uudestisyntyneinä")

    declensions.matches(Ins:: P, "uudestisyntynein")
    declensions.matches(Com:: P, "uudestisyntyneine")
  }


  "addDeclensions" should "handle case: rule: 48, word: vaate" in {
    val word = Word("vaate", 48, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vaate")
    declensions.matches(Gen:: S, "vaatteen")
    declensions.matches(Par:: S, "vaatetta")

    declensions.matches(Ill:: S, "vaatteeseen")

    declensions.matches(Ade:: S, "vaatteella")

    //Plural

    declensions.matches(Nom:: P, "vaatteet")
    declensions.matches(Gen:: P, "vaatteiden", "vaatteitten")
    declensions.matches(Par:: P, "vaatteita")

    declensions.matches(Ill:: P, "vaatteisiin", "vaatteihin")

    declensions.matches(Ade:: P, "vaatteilla")

    declensions.matches(Ess:: P, "vaatteina")

    declensions.matches(Ins:: P, "vaattein")
    declensions.matches(Com:: P, "vaatteine")
  }

  "addDeclensions" should "handle case: rule: 49, word: kannel" in {
    val word = Word("kannel", 49, Gradation("nt", "nn").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kannel")
    declensions.matches(Gen:: S, "kantelen")
    declensions.matches(Par:: S, "kannelta")

    declensions.matches(Ill:: S, "kanteleen")

    declensions.matches(Ade:: S, "kantelella")

    //Plural

    declensions.matches(Nom:: P, "kantelet")
    declensions.matches(Gen:: P, "kantelien", "kannelten")
    declensions.matches(Par:: P, "kantelia")

    declensions.matches(Ill:: P, "kanteliin")

    declensions.matches(Ade:: P, "kantelilla")

    declensions.matches(Ess:: P, "kantelina")

    declensions.matches(Ins:: P, "kantelin")
    declensions.matches(Com:: P, "kanteline")
  }

  "addDeclensions" should "handle case: rule: 49, word: kantele" in {
    val word = Word("kantele", 49, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kantele")
    declensions.matches(Gen:: S, "kanteleen")
    declensions.matches(Par:: S, "kanteletta")

    declensions.matches(Ill:: S, "kanteleeseen")

    declensions.matches(Ade:: S, "kanteleella")

    //Plural

    declensions.matches(Nom:: P, "kanteleet")
    declensions.matches(Gen:: P, "kanteleiden", "kanteleitten")
    declensions.matches(Par:: P, "kanteleita")

    declensions.matches(Ill:: P, "kanteleihin", "kanteleisiin")

    declensions.matches(Ade:: P, "kanteleilla")

    declensions.matches(Ess:: P, "kanteleina")

    declensions.matches(Ins:: P, "kantelein")
    declensions.matches(Com:: P, "kanteleine")
  }

  // APOSTROPHE
  //===================================================================

  "addDeclensions" should "handle case: vaaka, word: vaaka" in {
    val word = Word("vaaka", 9, Gradation("k", "").opt)
    val declensions = generateDeclensions(word)

    declensions.matches(Gen:: S, "vaa'an")
  }

  // GRADATION
  //===================================================================

  "addDeclensions" should "handle gradation: A, Strong, word: vadelmikko" in {
    val word = Word("vadelmikko", 4, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vadelmikko")
    declensions.matches(Gen:: S, "vadelmikon")
    declensions.matches(Par:: S, "vadelmikkoa")

    declensions.matches(Ill:: S, "vadelmikkoon")

    //Plural

    declensions.matches(Gen:: P, "vadelmikkojen", "vadelmikoitten", "vadelmikoiden")
    declensions.matches(Ill:: P, "vadelmikoihin", "vadelmikkoihin")

    declensions.matches(Ade:: P, "vadelmikoilla")
  }

  "addDeclensions" should "handle gradation: A, Weak, word: vahakas" in {
    val word = Word("vahakas", 41, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vahakas")
    declensions.matches(Gen:: S, "vahakkaan")
    declensions.matches(Par:: S, "vahakasta")

    declensions.matches(Ill:: S, "vahakkaaseen")

    //Plural

    declensions.matches(Gen:: P, "vahakkaiden", "vahakkaitten")
    declensions.matches(Ill:: P, "vahakkaisiin", "vahakkaihin")

    declensions.matches(Ade:: P, "vahakkailla")
  }

  "addDeclensions" should "handle gradation: B, Strong, word: vaippa" in {
    val word = Word("vaippa", 9, Gradation("pp", "p").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vaippa")
    declensions.matches(Gen:: S, "vaipan")
    declensions.matches(Par:: S, "vaippaa")

    declensions.matches(Ill:: S, "vaippaan")

    //Plural

    declensions.matches(Gen:: P, "vaippojen", "vaippain")
    declensions.matches(Ill:: P, "vaippoihin")

    declensions.matches(Ade:: P, "vaipoilla")
  }

  "addDeclensions" should "handle gradation: B, Weak, word: valpas" in {
    val word = Word("valpas", 41, Gradation("pp", "p").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "valpas")
    declensions.matches(Gen:: S, "valppaan")
    declensions.matches(Par:: S, "valpasta")

    declensions.matches(Ill:: S, "valppaaseen")

    //Plural

    declensions.matches(Gen:: P, "valppaiden", "valppaitten")
    declensions.matches(Ill:: P, "valppaisiin", "valppaihin")

    declensions.matches(Ade:: P, "valppailla")
  }

  "addDeclensions" should "handle gradation: C, Strong, word: valapatto" in {
    val word = Word("valapatto", 1, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "valapatto")
    declensions.matches(Gen:: S, "valapaton")
    declensions.matches(Par:: S, "valapattoa")

    declensions.matches(Ill:: S, "valapattoon")

    //Plural

    declensions.matches(Gen:: P, "valapattojen")
    declensions.matches(Ill:: P, "valapattoihin")

    declensions.matches(Ade:: P, "valapatoilla")
  }

  "addDeclensions" should "handle gradation: C, Weak, word: valkaisematon" in {
    val word = Word("valkaisematon", 34, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "valkaisematon")
    declensions.matches(Gen:: S, "valkaisemattoman")
    declensions.matches(Par:: S, "valkaisematonta")

    declensions.matches(Ill:: S, "valkaisemattomaan")

    //Plural

    declensions.matches(Gen:: P, "valkaisemattomien", "valkaisematonten")
    declensions.matches(Ill:: P, "valkaisemattomiin")

    declensions.matches(Ade:: P, "valkaisemattomilla")
  }

  "addDeclensions" should "handle gradation: D, Strong, word: valkaisematon" in {
    val word = Word("aika", 9, Gradation("k", "").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "aika")
    declensions.matches(Gen:: S, "ajan")
    declensions.matches(Par:: S, "aikaa")

    declensions.matches(Ill:: S, "aikaan")

    //Plural

    declensions.matches(Gen:: P, "aikojen", "aikain")
    declensions.matches(Ill:: P, "aikoihin")

    declensions.matches(Ade:: P, "ajoilla")
  }

  "addDeclensions" should "handle gradation: D, Weak, word: varas" in {
    val word = Word("varas", 41, Gradation("k", "").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "varas")
    declensions.matches(Gen:: S, "varkaan")
    declensions.matches(Par:: S, "varasta")

    declensions.matches(Ill:: S, "varkaaseen")

    //Plural

    declensions.matches(Gen:: P, "varkaiden", "varkaitten")
    declensions.matches(Ill:: P, "varkaisiin", "varkaihin")

    declensions.matches(Ade:: P, "varkailla")
  }

  "addDeclensions" should "handle gradation: E, Strong, word: varpu" in {
    val word = Word("varpu", 1, Gradation("p", "v").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "varpu")
    declensions.matches(Gen:: S, "varvun")
    declensions.matches(Par:: S, "varpua")

    declensions.matches(Ill:: S, "varpuun")

    //Plural

    declensions.matches(Gen:: P, "varpujen")
    declensions.matches(Ill:: P, "varpuihin")

    declensions.matches(Ade:: P, "varvuilla")
  }

  "addDeclensions" should "handle gradation: E, Weak, word: varvas" in {
    val word = Word("varvas", 41, Gradation("p", "v").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "varvas")
    declensions.matches(Gen:: S, "varpaan")
    declensions.matches(Par:: S, "varvasta")

    declensions.matches(Ill:: S, "varpaaseen")

    //Plural

    declensions.matches(Gen:: P, "varpaiden", "varpaitten")
    declensions.matches(Ill:: P, "varpaisiin", "varpaihin")

    declensions.matches(Ade:: P, "varpailla")
  }

  "addDeclensions" should "handle gradation: F, Strong, word: vastaveto" in {
    val word = Word("vastaveto", 1, Gradation("t", "d").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "vastaveto")
    declensions.matches(Gen:: S, "vastavedon")
    declensions.matches(Par:: S, "vastavetoa")

    declensions.matches(Ill:: S, "vastavetoon")

    //Plural

    declensions.matches(Gen:: P, "vastavetojen")
    declensions.matches(Ill:: P, "vastavetoihin")

    declensions.matches(Ade:: P, "vastavedoilla")
  }

  "addDeclensions" should "handle gradation: F, Weak, word: viihde" in {
    val word = Word("viihde", 48, Gradation("t", "d").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "viihde")
    declensions.matches(Gen:: S, "viihteen")
    declensions.matches(Par:: S, "viihdettä")

    declensions.matches(Ill:: S, "viihteeseen")

    //Plural

    declensions.matches(Gen:: P, "viihteiden", "viihteitten")
    declensions.matches(Ill:: P, "viihteisiin", "viihteihin")

    declensions.matches(Ade:: P, "viihteillä")
  }

  "addDeclensions" should "handle gradation: G, Strong, word: viikinki" in {
    val word = Word("viikinki", 5, Gradation("nk", "ng").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "viikinki")
    declensions.matches(Gen:: S, "viikingin")
    declensions.matches(Par:: S, "viikinkiä")

    declensions.matches(Ill:: S, "viikinkiin")

    //Plural

    declensions.matches(Gen:: P, "viikinkien")
    declensions.matches(Ill:: P, "viikinkeihin")

    declensions.matches(Ade:: P, "viikingeillä")
  }

  "addDeclensions" should "handle gradation: G, Weak, word: villakangas" in {
    val word = Word("villakangas", 41, Gradation("nk", "ng").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "villakangas")
    declensions.matches(Gen:: S, "villakankaan")
    declensions.matches(Par:: S, "villakangasta")

    declensions.matches(Ill:: S, "villakankaaseen")

    //Plural

    declensions.matches(Gen:: P, "villakankaiden", "villakankaitten")
    declensions.matches(Ill:: P, "villakankaisiin", "villakankaihin")

    declensions.matches(Ade:: P, "villakankailla")
  }

  "addDeclensions" should "handle gradation: H, Strong, word: ylempi" in {
    val word = Word("ylempi", 16, Gradation("mp", "mm").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "ylempi")
    declensions.matches(Gen:: S, "ylemmän")
    declensions.matches(Par:: S, "ylempää")

    declensions.matches(Ill:: S, "ylempään")

    //Plural

    declensions.matches(Gen:: P, "ylempien", "ylempäin")
    declensions.matches(Ill:: P, "ylempiin")

    declensions.matches(Ade:: P, "ylemmillä")
  }

  "addDeclensions" should "handle gradation: H, Weak, word: hammas" in {
    val word = Word("hammas", 41, Gradation("mp", "mm").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "hammas")
    declensions.matches(Gen:: S, "hampaan")
    declensions.matches(Par:: S, "hammasta")

    declensions.matches(Ill:: S, "hampaaseen")

    //Plural

    declensions.matches(Gen:: P, "hampaiden", "hampaitten")
    declensions.matches(Ill:: P, "hampaisiin", "hampaihin")

    declensions.matches(Ade:: P, "hampailla")
  }

  "addDeclensions" should "handle gradation: I, Strong, word: huolto" in {
    val word = Word("huolto", 1, Gradation("lt", "ll").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "huolto")
    declensions.matches(Gen:: S, "huollon")
    declensions.matches(Par:: S, "huoltoa")

    declensions.matches(Ill:: S, "huoltoon")

    //Plural

    declensions.matches(Gen:: P, "huoltojen")
    declensions.matches(Ill:: P, "huoltoihin")

    declensions.matches(Ade:: P, "huolloilla")
  }

  "addDeclensions" should "handle gradation: I, Weak, word: helle" in {
    val word = Word("helle", 48, Gradation("lt", "ll").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "helle")
    declensions.matches(Gen:: S, "helteen")
    declensions.matches(Par:: S, "hellettä")

    declensions.matches(Ill:: S, "helteeseen")

    //Plural

    declensions.matches(Gen:: P, "helteiden", "helteitten")
    declensions.matches(Ill:: P, "helteisiin", "helteihin")

    declensions.matches(Ade:: P, "helteillä")
  }

  "addDeclensions" should "handle gradation: J, Strong, word: hento" in {
    val word = Word("hento", 1, Gradation("nt", "nn").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "hento")
    declensions.matches(Gen:: S, "hennon")
    declensions.matches(Par:: S, "hentoa")

    declensions.matches(Ill:: S, "hentoon")

    //Plural

    declensions.matches(Gen:: P, "hentojen")
    declensions.matches(Ill:: P, "hentoihin")

    declensions.matches(Ade:: P, "hennoilla")
  }

  "addDeclensions" should "handle gradation: J, Weak, word: himmennin" in {
    val word = Word("himmennin", 33, Gradation("nt", "nn").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "himmennin")
    declensions.matches(Gen:: S, "himmentimen")
    declensions.matches(Par:: S, "himmennintä")

    declensions.matches(Ill:: S, "himmentimeen")

    //Plural

    declensions.matches(Gen:: P, "himmentimien", "himmenninten")
    declensions.matches(Ill:: P, "himmentimiin")

    declensions.matches(Ade:: P, "himmentimillä")
  }

  "addDeclensions" should "handle gradation: K, Strong, word: kaarto" in {
    val word = Word("kaarto", 1, Gradation("rt", "rr").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kaarto")
    declensions.matches(Gen:: S, "kaarron")
    declensions.matches(Par:: S, "kaartoa")

    declensions.matches(Ill:: S, "kaartoon")

    //Plural

    declensions.matches(Gen:: P, "kaartojen")
    declensions.matches(Ill:: P, "kaartoihin")

    declensions.matches(Ade:: P, "kaarroilla")
  }

  "addDeclensions" should "handle gradation: K, Weak, word: kierre" in {
    val word = Word("kierre", 48, Gradation("rt", "rr").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kierre")
    declensions.matches(Gen:: S, "kierteen")
    declensions.matches(Par:: S, "kierrettä")

    declensions.matches(Ill:: S, "kierteeseen")

    //Plural

    declensions.matches(Gen:: P, "kierteiden", "kierteitten")
    declensions.matches(Ill:: P, "kierteisiin", "kierteihin")

    declensions.matches(Ade:: P, "kierteillä")
  }

  "addDeclensions" should "handle gradation: L, Strong, word: kurki" in {
    val word = Word("kurki", 7, Gradation("k", "j").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "kurki")
    declensions.matches(Gen:: S, "kurjen")
    declensions.matches(Par:: S, "kurkea")

    declensions.matches(Ill:: S, "kurkeen")

    //Plural

    declensions.matches(Gen:: P, "kurkien")
    declensions.matches(Ill:: P, "kurkiin")

    declensions.matches(Ade:: P, "kurjilla")
  }

  "addDeclensions" should "handle gradation: L, Weak, word: lahje" in {
    val word = Word("lahje", 48, Gradation("k", "j").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "lahje")
    declensions.matches(Gen:: S, "lahkeen")
    declensions.matches(Par:: S, "lahjetta")

    declensions.matches(Ill:: S, "lahkeeseen")

    //Plural

    declensions.matches(Gen:: P, "lahkeiden", "lahkeitten")
    declensions.matches(Ill:: P, "lahkeisiin", "lahkeihin")

    declensions.matches(Ade:: P, "lahkeilla")
  }

  "addDeclensions" should "handle gradation: M, Strong, word: luku" in {
    val word = Word("luku", 1, Gradation("k", "v").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Nom:: S, "luku")
    declensions.matches(Gen:: S, "luvun")
    declensions.matches(Par:: S, "lukua")

    declensions.matches(Ill:: S, "lukuun")

    //Plural

    declensions.matches(Gen:: P, "lukujen")
    declensions.matches(Ill:: P, "lukuihin")

    declensions.matches(Ade:: P, "luvuilla")
  }
}
