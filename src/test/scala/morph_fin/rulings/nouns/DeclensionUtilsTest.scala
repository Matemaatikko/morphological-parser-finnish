package morph_fin.rulings.nouns

import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Abessive, Ablative, Adessive, Allative, Comitative, Elative, Essive, Genitive, Illative, Inessive, Instructive, Morphemes, Nominative, Noun, Partitive, Plural, Singular, Translative}
import morph_fin.rulings.rules.{DeclensionRule, Gradation, LoadAndParseNomineRules}


class DeclensionUtilsTest extends AnyFlatSpec with should.Matchers {


  import DeclensionUtils._
  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes == morphemes).map(_.word.toString).toSet == words.toSet, morphemes)

  "addDeclensions" should "handle case: rule: 1, word: järkky" in {
    val word = Word("järkky", 1, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "järkky")
    declensions.matches(Noun ~ Genitive ~ Singular, "järkyn")
    declensions.matches(Noun ~ Partitive ~ Singular, "järkkyä")
    //declensions.matches(Noun ~ Accusative ~ Singular, "järkky", "järkyn")

    declensions.matches(Noun ~ Inessive ~ Singular, "järkyssä")
    declensions.matches(Noun ~ Elative ~ Singular, "järkystä")
    declensions.matches(Noun ~ Illative ~ Singular, "järkkyyn")

    declensions.matches(Noun ~ Adessive ~ Singular, "järkyllä")
    declensions.matches(Noun ~ Ablative ~ Singular, "järkyltä")
    declensions.matches(Noun ~ Allative ~ Singular, "järkylle")

    declensions.matches(Noun ~ Essive ~ Singular, "järkkynä")
    declensions.matches(Noun ~ Translative ~ Singular, "järkyksi")
    declensions.matches(Noun ~ Abessive ~ Singular, "järkyttä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "järkyt")
    declensions.matches(Noun ~ Genitive ~ Plural, "järkkyjen")
    declensions.matches(Noun ~ Partitive ~ Plural, "järkkyjä")
    //declensions.matches(Noun ~ Accusative ~ Plural, "järkyt")

    declensions.matches(Noun ~ Inessive ~ Plural, "järkyissä")
    declensions.matches(Noun ~ Elative ~ Plural, "järkyistä")
    declensions.matches(Noun ~ Illative ~ Plural, "järkkyihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "järkyillä")
    declensions.matches(Noun ~ Ablative ~ Plural, "järkyiltä")
    declensions.matches(Noun ~ Allative ~ Plural, "järkyille")

    declensions.matches(Noun ~ Essive ~ Plural, "järkkyinä")
    declensions.matches(Noun ~ Translative ~ Plural, "järkyiksi")
    declensions.matches(Noun ~ Abessive ~ Plural, "järkyittä")
    declensions.matches(Noun ~ Instructive ~ Plural, "järkyin")
    declensions.matches(Noun ~ Comitative ~ Plural, "järkkyine")
  }

  "addDeclensions" should "handle case: rule: 2, word: jäämistö" in {
    val word = Word("jäämistö", 2, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "jäämistö")
    declensions.matches(Noun ~ Genitive ~ Singular, "jäämistön")
    declensions.matches(Noun ~ Partitive ~ Singular, "jäämistöä")

    declensions.matches(Noun ~ Illative ~ Singular, "jäämistöön")

    declensions.matches(Noun ~ Adessive ~ Singular, "jäämistöllä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "jäämistöt")
    declensions.matches(Noun ~ Genitive ~ Plural, "jäämistöjen", "jäämistöiden", "jäämistöitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "jäämistöjä", "jäämistöitä")

    declensions.matches(Noun ~ Illative ~ Plural, "jäämistöihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "jäämistöillä")

    declensions.matches(Noun ~ Instructive ~ Plural, "jäämistöin")
    declensions.matches(Noun ~ Comitative ~ Plural, "jäämistöine")
  }


  "addDeclensions" should "handle case: rule: 3, word: kaavio" in {
    val word = Word("kaavio", 3, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kaavio")
    declensions.matches(Noun ~ Genitive ~ Singular, "kaavion")
    declensions.matches(Noun ~ Partitive ~ Singular, "kaaviota")

    declensions.matches(Noun ~ Illative ~ Singular, "kaavioon")

    declensions.matches(Noun ~ Adessive ~ Singular, "kaaviolla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kaaviot")
    declensions.matches(Noun ~ Genitive ~ Plural, "kaavioiden", "kaavioitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "kaavioita")

    declensions.matches(Noun ~ Illative ~ Plural, "kaavioihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kaavioilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "kaavioin")
    declensions.matches(Noun ~ Comitative ~ Plural, "kaavioine")
  }

  "addDeclensions" should "handle case: rule: 4, word: kaislikko" in {
    val word = Word("kaislikko", 4, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kaislikko")
    declensions.matches(Noun ~ Genitive ~ Singular, "kaislikon")
    declensions.matches(Noun ~ Partitive ~ Singular, "kaislikkoa")

    declensions.matches(Noun ~ Illative ~ Singular, "kaislikkoon")

    declensions.matches(Noun ~ Adessive ~ Singular, "kaislikolla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kaislikot")
    declensions.matches(Noun ~ Genitive ~ Plural, "kaislikoiden", "kaislikoitten", "kaislikkojen")
    declensions.matches(Noun ~ Partitive ~ Plural, "kaislikoita", "kaislikkoja")

    declensions.matches(Noun ~ Illative ~ Plural, "kaislikkoihin", "kaislikoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kaislikoilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "kaislikoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "kaislikoine")
  }

  "addDeclensions" should "handle case: rule: 5, word: kalium" in {
    val word = Word("kalium", 5, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kalium")
    declensions.matches(Noun ~ Genitive ~ Singular, "kaliumin")
    declensions.matches(Noun ~ Partitive ~ Singular, "kaliumia")

    declensions.matches(Noun ~ Illative ~ Singular, "kaliumiin")

    declensions.matches(Noun ~ Adessive ~ Singular, "kaliumilla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kaliumit")
    declensions.matches(Noun ~ Genitive ~ Plural, "kaliumien")
    declensions.matches(Noun ~ Partitive ~ Plural, "kaliumeja")

    declensions.matches(Noun ~ Illative ~ Plural, "kaliumeihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kaliumeilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "kaliumein")
    declensions.matches(Noun ~ Comitative ~ Plural, "kaliumeine")
  }

  "addDeclensions" should "handle case: rule: 5, word: kalkki" in {
    val word = Word("kalkki", 5, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kalkki")
    declensions.matches(Noun ~ Genitive ~ Singular, "kalkin")
    declensions.matches(Noun ~ Partitive ~ Singular, "kalkkia")

    declensions.matches(Noun ~ Illative ~ Singular, "kalkkiin")

    declensions.matches(Noun ~ Adessive ~ Singular, "kalkilla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kalkit")
    declensions.matches(Noun ~ Genitive ~ Plural, "kalkkien")
    declensions.matches(Noun ~ Partitive ~ Plural, "kalkkeja")

    declensions.matches(Noun ~ Illative ~ Plural, "kalkkeihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kalkeilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "kalkein")
    declensions.matches(Noun ~ Comitative ~ Plural, "kalkkeine")
  }

  "addDeclensions" should "handle case: rule: 6, word: kalori" in {
    val word = Word("kalori", 6, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kalori")
    declensions.matches(Noun ~ Genitive ~ Singular, "kalorin")
    declensions.matches(Noun ~ Partitive ~ Singular, "kaloria")

    declensions.matches(Noun ~ Illative ~ Singular, "kaloriin")

    declensions.matches(Noun ~ Adessive ~ Singular, "kalorilla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kalorit")
    declensions.matches(Noun ~ Genitive ~ Plural, "kalorien", "kaloreiden", "kaloreitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "kaloreja", "kaloreita")

    declensions.matches(Noun ~ Illative ~ Plural, "kaloreihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kaloreilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "kalorein")
    declensions.matches(Noun ~ Comitative ~ Plural, "kaloreine")
  }

  "addDeclensions" should "handle case: rule: 6, word: kalterit (plural parsing)" in {
    val word = Word("kalterit", 6, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kalteri")
    declensions.matches(Noun ~ Genitive ~ Singular, "kalterin")
    declensions.matches(Noun ~ Partitive ~ Singular, "kalteria")

    declensions.matches(Noun ~ Illative ~ Singular, "kalteriin")

    declensions.matches(Noun ~ Adessive ~ Singular, "kalterilla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kalterit")
    declensions.matches(Noun ~ Genitive ~ Plural, "kalterien", "kaltereiden", "kaltereitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "kaltereita", "kaltereja")

    declensions.matches(Noun ~ Illative ~ Plural, "kaltereihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kaltereilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "kalterein")
    declensions.matches(Noun ~ Comitative ~ Plural, "kaltereine")
  }

  "addDeclensions" should "handle case: rule: 7, word: kanki" in {
    val word = Word("kanki", 7, Gradation("nk", "ng").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kanki")
    declensions.matches(Noun ~ Genitive ~ Singular, "kangen")
    declensions.matches(Noun ~ Partitive ~ Singular, "kankea")

    declensions.matches(Noun ~ Illative ~ Singular, "kankeen")

    declensions.matches(Noun ~ Adessive ~ Singular, "kangella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kanget")
    declensions.matches(Noun ~ Genitive ~ Plural, "kankien")
    declensions.matches(Noun ~ Partitive ~ Plural, "kankia")

    declensions.matches(Noun ~ Illative ~ Plural, "kankiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kangilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "kangin")
    declensions.matches(Noun ~ Comitative ~ Plural, "kankine")
  }

  "addDeclensions" should "handle case: rule: 8, word: manne" in {
    val word = Word("manne", 8, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "manne")
    declensions.matches(Noun ~ Genitive ~ Singular, "mannen")
    declensions.matches(Noun ~ Partitive ~ Singular, "mannea")

    declensions.matches(Noun ~ Illative ~ Singular, "manneen")

    declensions.matches(Noun ~ Adessive ~ Singular, "mannella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "mannet")
    declensions.matches(Noun ~ Genitive ~ Plural, "mannejen", "mannein")
    declensions.matches(Noun ~ Partitive ~ Plural, "manneja")

    declensions.matches(Noun ~ Illative ~ Plural, "manneihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "manneilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "mannein")
    declensions.matches(Noun ~ Comitative ~ Plural, "manneine")
  }

  "addDeclensions" should "handle case: rule: 9, word: marja-aika" in {
    val word = Word("marja-aika", 9, Gradation("k", "").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "marja-aika")
    declensions.matches(Noun ~ Genitive ~ Singular, "marja-ajan")
    declensions.matches(Noun ~ Partitive ~ Singular, "marja-aikaa")

    declensions.matches(Noun ~ Illative ~ Singular, "marja-aikaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "marja-ajalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "marja-ajat")
    declensions.matches(Noun ~ Genitive ~ Plural, "marja-aikojen", "marja-aikain")
    declensions.matches(Noun ~ Partitive ~ Plural, "marja-aikoja")

    declensions.matches(Noun ~ Illative ~ Plural, "marja-aikoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "marja-ajoilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "marja-ajoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "marja-aikoine")
  }

  "addDeclensions" should "handle case: rule: 10, word: matala" in {
    val word = Word("matala", 10, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "matala")
    declensions.matches(Noun ~ Genitive ~ Singular, "matalan")
    declensions.matches(Noun ~ Partitive ~ Singular, "matalaa")

    declensions.matches(Noun ~ Illative ~ Singular, "matalaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "matalalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "matalat")
    declensions.matches(Noun ~ Genitive ~ Plural, "matalien", "matalain")
    declensions.matches(Noun ~ Partitive ~ Plural, "matalia")

    declensions.matches(Noun ~ Illative ~ Plural, "mataliin")

    declensions.matches(Noun ~ Adessive ~ Plural, "matalilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "matalin")
    declensions.matches(Noun ~ Comitative ~ Plural, "mataline")
  }

  "addDeclensions" should "handle case: rule: 11, word: papana" in {
    val word = Word("papana", 11, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "papana")
    declensions.matches(Noun ~ Genitive ~ Singular, "papanan")
    declensions.matches(Noun ~ Partitive ~ Singular, "papanaa")

    declensions.matches(Noun ~ Illative ~ Singular, "papanaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "papanalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "papanat")
    declensions.matches(Noun ~ Genitive ~ Plural, "papanien", "papanoiden", "papanoitten", "papanojen", "papanain")
    declensions.matches(Noun ~ Partitive ~ Plural, "papania", "papanoita", "papanoja")

    declensions.matches(Noun ~ Illative ~ Plural, "papanoihin", "papaniin")

    declensions.matches(Noun ~ Adessive ~ Plural, "papanilla", "papanoilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "papanin", "papanoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "papanine")
  }

  "addDeclensions" should "handle case: rule: 12, word: passiivat = velat (plural)" in {
    val word = Word("passiivat", 12, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "passiiva")
    declensions.matches(Noun ~ Genitive ~ Singular, "passiivan")
    declensions.matches(Noun ~ Partitive ~ Singular, "passiivaa")

    declensions.matches(Noun ~ Illative ~ Singular, "passiivaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "passiivalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "passiivat")
    declensions.matches(Noun ~ Genitive ~ Plural, "passiivoiden", "passiivoitten", "passiivain")
    declensions.matches(Noun ~ Partitive ~ Plural, "passiivoita")

    declensions.matches(Noun ~ Illative ~ Plural, "passiivoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "passiivoilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "passiivoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "passiivoine")
  }

  "addDeclensions" should "handle case: rule: 12, word: pasteija (Singular)" in {
    val word = Word("pasteija", 12, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "pasteija")
    declensions.matches(Noun ~ Genitive ~ Singular, "pasteijan")
    declensions.matches(Noun ~ Partitive ~ Singular, "pasteijaa")

    declensions.matches(Noun ~ Illative ~ Singular, "pasteijaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "pasteijalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "pasteijat")
    declensions.matches(Noun ~ Genitive ~ Plural, "pasteijoiden", "pasteijoitten", "pasteijain")
    declensions.matches(Noun ~ Partitive ~ Plural, "pasteijoita")

    declensions.matches(Noun ~ Illative ~ Plural, "pasteijoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "pasteijoilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "pasteijoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "pasteijoine")
  }

  "addDeclensions" should "handle case: rule: 13, word: perusta" in {
    val word = Word("perusta", 13, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "perusta")
    declensions.matches(Noun ~ Genitive ~ Singular, "perustan")
    declensions.matches(Noun ~ Partitive ~ Singular, "perustaa")

    declensions.matches(Noun ~ Illative ~ Singular, "perustaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "perustalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "perustat")
    declensions.matches(Noun ~ Genitive ~ Plural, "perustojen", "perustoiden", "perustoitten", "perustain")
    declensions.matches(Noun ~ Partitive ~ Plural, "perustoja", "perustoita")

    declensions.matches(Noun ~ Illative ~ Plural, "perustoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "perustoilla")

    declensions.matches(Noun ~ Instructive ~ Plural, "perustoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "perustoine")
  }

  "addDeclensions" should "handle case: rule: 14, word: pohatta" in {
    val word = Word("pohatta", 14, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "pohatta")
    declensions.matches(Noun ~ Genitive ~ Singular, "pohatan")
    declensions.matches(Noun ~ Partitive ~ Singular, "pohattaa")

    declensions.matches(Noun ~ Illative ~ Singular, "pohattaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "pohatalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "pohatat")
    declensions.matches(Noun ~ Genitive ~ Plural, "pohattojen", "pohatoiden", "pohatoitten", "pohattain")
    declensions.matches(Noun ~ Partitive ~ Plural, "pohattoja", "pohatoita")

    declensions.matches(Noun ~ Illative ~ Plural, "pohattoihin", "pohatoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "pohatoilla")

    declensions.matches(Noun ~ Essive ~ Plural, "pohatoina", "pohattoina")

    declensions.matches(Noun ~ Instructive ~ Plural, "pohatoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "pohatoine", "pohattoine")
  }

  "addDeclensions" should "handle case: rule: 15, word: hilpeä" in {
    val word = Word("hilpeä", 15, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "hilpeä")
    declensions.matches(Noun ~ Genitive ~ Singular, "hilpeän")
    declensions.matches(Noun ~ Partitive ~ Singular, "hilpeää")

    declensions.matches(Noun ~ Illative ~ Singular, "hilpeään")

    declensions.matches(Noun ~ Adessive ~ Singular, "hilpeällä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "hilpeät")
    declensions.matches(Noun ~ Genitive ~ Plural, "hilpeiden", "hilpeitten", "hilpeäin")
    declensions.matches(Noun ~ Partitive ~ Plural, "hilpeitä")

    declensions.matches(Noun ~ Illative ~ Plural, "hilpeihin", "hilpeisiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "hilpeillä")

    declensions.matches(Noun ~ Essive ~ Plural, "hilpeinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "hilpein")
    declensions.matches(Noun ~ Comitative ~ Plural, "hilpeine")
  }

  "addDeclensions" should "handle case: rule: 16, word: likempi" in {
    val word = Word("likempi", 16, Gradation("mp", "mm").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "likempi")
    declensions.matches(Noun ~ Genitive ~ Singular, "likemmän")
    declensions.matches(Noun ~ Partitive ~ Singular, "likempää")

    declensions.matches(Noun ~ Illative ~ Singular, "likempään")

    declensions.matches(Noun ~ Adessive ~ Singular, "likemmällä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "likemmät")
    declensions.matches(Noun ~ Genitive ~ Plural, "likempien", "likempäin")
    declensions.matches(Noun ~ Partitive ~ Plural, "likempiä")

    declensions.matches(Noun ~ Illative ~ Plural, "likempiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "likemmillä")

    declensions.matches(Noun ~ Essive ~ Plural, "likempinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "likemmin")
    declensions.matches(Noun ~ Comitative ~ Plural, "likempine")
  }

  "addDeclensions" should "handle case: rule: 17, word: palttoo" in {
    val word = Word("palttoo", 17, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "palttoo")
    declensions.matches(Noun ~ Genitive ~ Singular, "palttoon")
    declensions.matches(Noun ~ Partitive ~ Singular, "palttoota")

    declensions.matches(Noun ~ Illative ~ Singular, "palttooseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "palttoolla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "palttoot")
    declensions.matches(Noun ~ Genitive ~ Plural, "palttoiden", "palttoitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "palttoita")

    declensions.matches(Noun ~ Illative ~ Plural, "palttoisiin", "palttoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "palttoilla")

    declensions.matches(Noun ~ Essive ~ Plural, "palttoina")

    declensions.matches(Noun ~ Instructive ~ Plural, "palttoin")
    declensions.matches(Noun ~ Comitative ~ Plural, "palttoine")
  }

  "addDeclensions" should "handle case: rule: 18, word: perjantai" in {
    val word = Word("perjantai", 18, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "perjantai")
    declensions.matches(Noun ~ Genitive ~ Singular, "perjantain")
    declensions.matches(Noun ~ Partitive ~ Singular, "perjantaita")

    declensions.matches(Noun ~ Illative ~ Singular, "perjantaihin")

    declensions.matches(Noun ~ Adessive ~ Singular, "perjantailla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "perjantait")
    declensions.matches(Noun ~ Genitive ~ Plural, "perjantaiden", "perjantaitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "perjantaita")

    declensions.matches(Noun ~ Illative ~ Plural, "perjantaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "perjantailla")

    declensions.matches(Noun ~ Essive ~ Plural, "perjantaina")

    declensions.matches(Noun ~ Instructive ~ Plural, "perjantain")
    declensions.matches(Noun ~ Comitative ~ Plural, "perjantaine")
  }

  "addDeclensions" should "handle case: rule: 19, word: tie" in {
    val word = Word("tie", 19, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "tie")
    declensions.matches(Noun ~ Genitive ~ Singular, "tien")
    declensions.matches(Noun ~ Partitive ~ Singular, "tietä")

    declensions.matches(Noun ~ Illative ~ Singular, "tiehen")

    declensions.matches(Noun ~ Adessive ~ Singular, "tiellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "tiet")
    declensions.matches(Noun ~ Genitive ~ Plural, "teiden", "teitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "teitä")

    declensions.matches(Noun ~ Illative ~ Plural, "teihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "teillä")

    declensions.matches(Noun ~ Essive ~ Plural, "teinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "tein")
    declensions.matches(Noun ~ Comitative ~ Plural, "teine")
  }

  "addDeclensions" should "handle case: rule: 21, word: coupé" in {
    val word = Word("coupé", 21, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "coupé")
    declensions.matches(Noun ~ Genitive ~ Singular, "coupén")
    declensions.matches(Noun ~ Partitive ~ Singular, "coupéta")

    declensions.matches(Noun ~ Illative ~ Singular, "coupéhen")

    declensions.matches(Noun ~ Adessive ~ Singular, "coupélla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "coupét")
    declensions.matches(Noun ~ Genitive ~ Plural, "coupéiden", "coupéitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "coupéita")

    declensions.matches(Noun ~ Illative ~ Plural, "coupéihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "coupéilla")

    declensions.matches(Noun ~ Essive ~ Plural, "coupéina")

    declensions.matches(Noun ~ Instructive ~ Plural, "coupéin")
    declensions.matches(Noun ~ Comitative ~ Plural, "coupéine")
  }

  "addDeclensions" should "handle case: rule: 22, word: nougat" in {
    val word = Word("nougat", 22, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "nougat")
    declensions.matches(Noun ~ Genitive ~ Singular, "nougat'n")
    declensions.matches(Noun ~ Partitive ~ Singular, "nougat'ta")

    declensions.matches(Noun ~ Illative ~ Singular, "nougat'hen")

    declensions.matches(Noun ~ Adessive ~ Singular, "nougat'lla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "nougat't")
    declensions.matches(Noun ~ Genitive ~ Plural, "nougat'iden", "nougat'itten")
    declensions.matches(Noun ~ Partitive ~ Plural, "nougat'ita")

    declensions.matches(Noun ~ Illative ~ Plural, "nougat'ihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "nougat'illa")

    declensions.matches(Noun ~ Essive ~ Plural, "nougat'ina")

    declensions.matches(Noun ~ Instructive ~ Plural, "nougat'in")
    declensions.matches(Noun ~ Comitative ~ Plural, "nougat'ine")
  }

  "addDeclensions" should "handle case: rule: 23, word: tuli" in {
    val word = Word("tuli", 23, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "tuli")
    declensions.matches(Noun ~ Genitive ~ Singular, "tulen")
    declensions.matches(Noun ~ Partitive ~ Singular, "tulta")

    declensions.matches(Noun ~ Illative ~ Singular, "tuleen")

    declensions.matches(Noun ~ Adessive ~ Singular, "tulella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "tulet")
    declensions.matches(Noun ~ Genitive ~ Plural, "tulien")
    declensions.matches(Noun ~ Partitive ~ Plural, "tulia")

    declensions.matches(Noun ~ Illative ~ Plural, "tuliin")

    declensions.matches(Noun ~ Adessive ~ Plural, "tulilla")

    declensions.matches(Noun ~ Essive ~ Plural, "tulina")

    declensions.matches(Noun ~ Instructive ~ Plural, "tulin")
    declensions.matches(Noun ~ Comitative ~ Plural, "tuline")
  }

  "addDeclensions" should "handle case: rule: 24, word: hiili" in {
    val word = Word("hiili", 24, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "hiili")
    declensions.matches(Noun ~ Genitive ~ Singular, "hiilen")
    declensions.matches(Noun ~ Partitive ~ Singular, "hiiltä")

    declensions.matches(Noun ~ Illative ~ Singular, "hiileen")

    declensions.matches(Noun ~ Adessive ~ Singular, "hiilellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "hiilet")
    declensions.matches(Noun ~ Genitive ~ Plural, "hiilien", "hiilten")
    declensions.matches(Noun ~ Partitive ~ Plural, "hiiliä")

    declensions.matches(Noun ~ Illative ~ Plural, "hiiliin")

    declensions.matches(Noun ~ Adessive ~ Plural, "hiilillä")

    declensions.matches(Noun ~ Essive ~ Plural, "hiilinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "hiilin")
    declensions.matches(Noun ~ Comitative ~ Plural, "hiiline")
  }

  "addDeclensions" should "handle case: rule: 25, word: lumi" in {
    val word = Word("lumi", 25, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "lumi")
    declensions.matches(Noun ~ Genitive ~ Singular, "lumen")
    declensions.matches(Noun ~ Partitive ~ Singular, "lunta", "lumea")

    declensions.matches(Noun ~ Illative ~ Singular, "lumeen")

    declensions.matches(Noun ~ Adessive ~ Singular, "lumella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "lumet")
    declensions.matches(Noun ~ Genitive ~ Plural, "lumien", "lunten")
    declensions.matches(Noun ~ Partitive ~ Plural, "lumia")

    declensions.matches(Noun ~ Illative ~ Plural, "lumiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "lumilla")

    declensions.matches(Noun ~ Essive ~ Plural, "lumina")

    declensions.matches(Noun ~ Instructive ~ Plural, "lumin")
    declensions.matches(Noun ~ Comitative ~ Plural, "lumine")
  }

  "addDeclensions" should "handle case: rule: 26, word: mieli" in {
    val word = Word("mieli", 26, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "mieli")
    declensions.matches(Noun ~ Genitive ~ Singular, "mielen")
    declensions.matches(Noun ~ Partitive ~ Singular, "mieltä")

    declensions.matches(Noun ~ Illative ~ Singular, "mieleen")

    declensions.matches(Noun ~ Adessive ~ Singular, "mielellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "mielet")
    declensions.matches(Noun ~ Genitive ~ Plural, "mielten", "mielien")
    declensions.matches(Noun ~ Partitive ~ Plural, "mieliä")

    declensions.matches(Noun ~ Illative ~ Plural, "mieliin")

    declensions.matches(Noun ~ Adessive ~ Plural, "mielillä")

    declensions.matches(Noun ~ Essive ~ Plural, "mielinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "mielin")
    declensions.matches(Noun ~ Comitative ~ Plural, "mieline")
  }

  "addDeclensions" should "handle case: rule: 27, word: reisi" in {
    val word = Word("reisi", 27, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "reisi")
    declensions.matches(Noun ~ Genitive ~ Singular, "reiden")
    declensions.matches(Noun ~ Partitive ~ Singular, "reittä")

    declensions.matches(Noun ~ Illative ~ Singular, "reiteen")

    declensions.matches(Noun ~ Adessive ~ Singular, "reidellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "reidet")
    declensions.matches(Noun ~ Genitive ~ Plural, "reisien", "reitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "reisiä")

    declensions.matches(Noun ~ Illative ~ Plural, "reisiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "reisillä")

    declensions.matches(Noun ~ Essive ~ Plural, "reisinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "reisin")
    declensions.matches(Noun ~ Comitative ~ Plural, "reisine")
  }

  "addDeclensions" should "handle case: rule: 28, word: virsi" in {
    val word = Word("virsi", 28, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "virsi")
    declensions.matches(Noun ~ Genitive ~ Singular, "virren")
    declensions.matches(Noun ~ Partitive ~ Singular, "virttä")

    declensions.matches(Noun ~ Illative ~ Singular, "virteen")

    declensions.matches(Noun ~ Adessive ~ Singular, "virrellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "virret")
    declensions.matches(Noun ~ Genitive ~ Plural, "virsien", "virtten")
    declensions.matches(Noun ~ Partitive ~ Plural, "virsiä")

    declensions.matches(Noun ~ Illative ~ Plural, "virsiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "virsillä")

    declensions.matches(Noun ~ Essive ~ Plural, "virsinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "virsin")
    declensions.matches(Noun ~ Comitative ~ Plural, "virsine")
  }

  "addDeclensions" should "handle case: rule: 28, word: jälsi" in {
    val word = Word("jälsi", 28, Gradation("lt", "ll").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "jälsi")
    declensions.matches(Noun ~ Genitive ~ Singular, "jällen")
    declensions.matches(Noun ~ Partitive ~ Singular, "jälttä")

    declensions.matches(Noun ~ Illative ~ Singular, "jälteen")

    declensions.matches(Noun ~ Adessive ~ Singular, "jällellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "jället")
    declensions.matches(Noun ~ Genitive ~ Plural, "jälsien", "jältten")
    declensions.matches(Noun ~ Partitive ~ Plural, "jälsiä")

    declensions.matches(Noun ~ Illative ~ Plural, "jälsiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "jälsillä")

    declensions.matches(Noun ~ Essive ~ Plural, "jälsinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "jälsin")
    declensions.matches(Noun ~ Comitative ~ Plural, "jälsine")
  }

  "addDeclensions" should "handle case: rule: 29, word: lapsi" in {
    val word = Word("lapsi", 29, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "lapsi")
    declensions.matches(Noun ~ Genitive ~ Singular, "lapsen")
    declensions.matches(Noun ~ Partitive ~ Singular, "lasta")

    declensions.matches(Noun ~ Illative ~ Singular, "lapseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "lapsella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "lapset")
    declensions.matches(Noun ~ Genitive ~ Plural, "lapsien", "lasten")
    declensions.matches(Noun ~ Partitive ~ Plural, "lapsia")

    declensions.matches(Noun ~ Illative ~ Plural, "lapsiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "lapsilla")

    declensions.matches(Noun ~ Essive ~ Plural, "lapsina")

    declensions.matches(Noun ~ Instructive ~ Plural, "lapsin")
    declensions.matches(Noun ~ Comitative ~ Plural, "lapsine")
  }

  "addDeclensions" should "handle case: rule: 30, word: peitsi" in {
    val word = Word("peitsi", 30, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "peitsi")
    declensions.matches(Noun ~ Genitive ~ Singular, "peitsen")
    declensions.matches(Noun ~ Partitive ~ Singular, "peistä")

    declensions.matches(Noun ~ Illative ~ Singular, "peitseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "peitsellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "peitset")
    declensions.matches(Noun ~ Genitive ~ Plural, "peitsien", "peisten")
    declensions.matches(Noun ~ Partitive ~ Plural, "peitsiä")

    declensions.matches(Noun ~ Illative ~ Plural, "peitsiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "peitsillä")

    declensions.matches(Noun ~ Essive ~ Plural, "peitsinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "peitsin")
    declensions.matches(Noun ~ Comitative ~ Plural, "peitsine")
  }

  "addDeclensions" should "handle case: rule: 31, word: yksi" in {
    val word = Word("yksi", 31, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "yksi")
    declensions.matches(Noun ~ Genitive ~ Singular, "yhden")
    declensions.matches(Noun ~ Partitive ~ Singular, "yhtä")

    declensions.matches(Noun ~ Illative ~ Singular, "yhteen")

    declensions.matches(Noun ~ Adessive ~ Singular, "yhdellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "yhdet")
    declensions.matches(Noun ~ Genitive ~ Plural, "yksien")
    declensions.matches(Noun ~ Partitive ~ Plural, "yksiä")

    declensions.matches(Noun ~ Illative ~ Plural, "yksiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "yksillä")

    declensions.matches(Noun ~ Essive ~ Plural, "yksinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "yksin")
    declensions.matches(Noun ~ Comitative ~ Plural, "yksine")
  }

  "addDeclensions" should "handle case: rule: 32, word: aamen" in {
    val word = Word("aamen", 32, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "aamen")
    declensions.matches(Noun ~ Genitive ~ Singular, "aamenen")
    declensions.matches(Noun ~ Partitive ~ Singular, "aamenta")

    declensions.matches(Noun ~ Illative ~ Singular, "aameneen")

    declensions.matches(Noun ~ Adessive ~ Singular, "aamenella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "aamenet")
    declensions.matches(Noun ~ Genitive ~ Plural, "aamenien", "aamenten")
    declensions.matches(Noun ~ Partitive ~ Plural, "aamenia")

    declensions.matches(Noun ~ Illative ~ Plural, "aameniin")

    declensions.matches(Noun ~ Adessive ~ Plural, "aamenilla")

    declensions.matches(Noun ~ Essive ~ Plural, "aamenina")

    declensions.matches(Noun ~ Instructive ~ Plural, "aamenin")
    declensions.matches(Noun ~ Comitative ~ Plural, "aamenine")
  }

  "addDeclensions" should "handle case: rule: 33, word: ahdin" in {
    val word = Word("ahdin", 33, Gradation("t", "d").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "ahdin")
    declensions.matches(Noun ~ Genitive ~ Singular, "ahtimen")
    declensions.matches(Noun ~ Partitive ~ Singular, "ahdinta")

    declensions.matches(Noun ~ Illative ~ Singular, "ahtimeen")

    declensions.matches(Noun ~ Adessive ~ Singular, "ahtimella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "ahtimet")
    declensions.matches(Noun ~ Genitive ~ Plural, "ahdinten", "ahtimien")
    declensions.matches(Noun ~ Partitive ~ Plural, "ahtimia")

    declensions.matches(Noun ~ Illative ~ Plural, "ahtimiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "ahtimilla")

    declensions.matches(Noun ~ Essive ~ Plural, "ahtimina")

    declensions.matches(Noun ~ Instructive ~ Plural, "ahtimin")
    declensions.matches(Noun ~ Comitative ~ Plural, "ahtimine")
  }

  "addDeclensions" should "handle case: rule: 34, word: aineeton" in {
    val word = Word("aineeton", 34, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "aineeton")
    declensions.matches(Noun ~ Genitive ~ Singular, "aineettoman")
    declensions.matches(Noun ~ Partitive ~ Singular, "aineetonta")

    declensions.matches(Noun ~ Illative ~ Singular, "aineettomaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "aineettomalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "aineettomat")
    declensions.matches(Noun ~ Genitive ~ Plural, "aineetonten", "aineettomien")
    declensions.matches(Noun ~ Partitive ~ Plural, "aineettomia")

    declensions.matches(Noun ~ Illative ~ Plural, "aineettomiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "aineettomilla")

    declensions.matches(Noun ~ Essive ~ Plural, "aineettomina")

    declensions.matches(Noun ~ Instructive ~ Plural, "aineettomin")
    declensions.matches(Noun ~ Comitative ~ Plural, "aineettomine")
  }

  "addDeclensions" should "handle case: rule: 34, word: hapan" in {
    val word = Word("hapan", 34, Gradation("pp", "p").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "hapan")
    declensions.matches(Noun ~ Genitive ~ Singular, "happaman")
  }

  "addDeclensions" should "handle case: rule: 35, word: lämmin" in {
    val word = Word("lämmin", 35, Gradation("mp", "mm").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "lämmin")
    declensions.matches(Noun ~ Genitive ~ Singular, "lämpimän")
    declensions.matches(Noun ~ Partitive ~ Singular, "lämmintä")

    declensions.matches(Noun ~ Illative ~ Singular, "lämpimään")

    declensions.matches(Noun ~ Adessive ~ Singular, "lämpimällä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "lämpimät")
    declensions.matches(Noun ~ Genitive ~ Plural, "lämpimien", "lämpimäin")
    declensions.matches(Noun ~ Partitive ~ Plural, "lämpimiä")

    declensions.matches(Noun ~ Illative ~ Plural, "lämpimiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "lämpimillä")

    declensions.matches(Noun ~ Essive ~ Plural, "lämpiminä")

    declensions.matches(Noun ~ Instructive ~ Plural, "lämpimin")
    declensions.matches(Noun ~ Comitative ~ Plural, "lämpimine")
  }

  "addDeclensions" should "handle case: rule: 36, word: parhain" in {
    val word = Word("parhain", 36, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "parhain")
    declensions.matches(Noun ~ Genitive ~ Singular, "parhaimman")
    declensions.matches(Noun ~ Partitive ~ Singular, "parhainta")

    declensions.matches(Noun ~ Illative ~ Singular, "parhaimpaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "parhaimmalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "parhaimmat")
    declensions.matches(Noun ~ Genitive ~ Plural, "parhaimpien", "parhainten", "parhaimpain")
    declensions.matches(Noun ~ Partitive ~ Plural, "parhaimpia")

    declensions.matches(Noun ~ Illative ~ Plural, "parhaimpiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "parhaimmilla")

    declensions.matches(Noun ~ Essive ~ Plural, "parhaimpina")

    declensions.matches(Noun ~ Instructive ~ Plural, "parhaimmin")
    declensions.matches(Noun ~ Comitative ~ Plural, "parhaimpine")
  }

  "addDeclensions" should "handle case: rule: 37, word: vasen" in {
    val word = Word("vasen", 37, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vasen")
    declensions.matches(Noun ~ Genitive ~ Singular, "vasemman")
    declensions.matches(Noun ~ Partitive ~ Singular, "vasenta", "vasempaa")

    declensions.matches(Noun ~ Illative ~ Singular, "vasempaan")

    declensions.matches(Noun ~ Adessive ~ Singular, "vasemmalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "vasemmat")
    declensions.matches(Noun ~ Genitive ~ Plural, "vasempien", "vasenten", "vasempain")
    declensions.matches(Noun ~ Partitive ~ Plural, "vasempia")

    declensions.matches(Noun ~ Illative ~ Plural, "vasempiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vasemmilla")

    declensions.matches(Noun ~ Essive ~ Plural, "vasempina")

    declensions.matches(Noun ~ Instructive ~ Plural, "vasemmin")
    declensions.matches(Noun ~ Comitative ~ Plural, "vasempine")
  }

  "addDeclensions" should "handle case: rule: 38, word: vastuullinen" in {
    val word = Word("vastuullinen", 38, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vastuullinen")
    declensions.matches(Noun ~ Genitive ~ Singular, "vastuullisen")
    declensions.matches(Noun ~ Partitive ~ Singular, "vastuullista")

    declensions.matches(Noun ~ Illative ~ Singular, "vastuulliseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "vastuullisella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "vastuulliset")
    declensions.matches(Noun ~ Genitive ~ Plural, "vastuullisten", "vastuullisien")
    declensions.matches(Noun ~ Partitive ~ Plural, "vastuullisia")

    declensions.matches(Noun ~ Illative ~ Plural, "vastuullisiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vastuullisilla")

    declensions.matches(Noun ~ Essive ~ Plural, "vastuullisina")

    declensions.matches(Noun ~ Instructive ~ Plural, "vastuullisin")
    declensions.matches(Noun ~ Comitative ~ Plural, "vastuullisine")
  }

  "addDeclensions" should "handle case: rule: 39, word: vavahdus" in {
    val word = Word("vavahdus", 39, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vavahdus")
    declensions.matches(Noun ~ Genitive ~ Singular, "vavahduksen")
    declensions.matches(Noun ~ Partitive ~ Singular, "vavahdusta")

    declensions.matches(Noun ~ Illative ~ Singular, "vavahdukseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "vavahduksella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "vavahdukset")
    declensions.matches(Noun ~ Genitive ~ Plural, "vavahdusten", "vavahduksien")
    declensions.matches(Noun ~ Partitive ~ Plural, "vavahduksia")

    declensions.matches(Noun ~ Illative ~ Plural, "vavahduksiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vavahduksilla")

    declensions.matches(Noun ~ Essive ~ Plural, "vavahduksina")

    declensions.matches(Noun ~ Instructive ~ Plural, "vavahduksin")
    declensions.matches(Noun ~ Comitative ~ Plural, "vavahduksine")
  }

  "addDeclensions" should "handle case: rule: 40, word: velkaisuus" in {
    val word = Word("velkaisuus", 40, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "velkaisuus")
    declensions.matches(Noun ~ Genitive ~ Singular, "velkaisuuden")
    declensions.matches(Noun ~ Partitive ~ Singular, "velkaisuutta")

    declensions.matches(Noun ~ Illative ~ Singular, "velkaisuuteen")

    declensions.matches(Noun ~ Adessive ~ Singular, "velkaisuudella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "velkaisuudet")
    declensions.matches(Noun ~ Genitive ~ Plural, "velkaisuuksien")
    declensions.matches(Noun ~ Partitive ~ Plural, "velkaisuuksia")

    declensions.matches(Noun ~ Illative ~ Plural, "velkaisuuksiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "velkaisuuksilla")

    declensions.matches(Noun ~ Essive ~ Plural, "velkaisuuksina")

    declensions.matches(Noun ~ Instructive ~ Plural, "velkaisuuksin")
    declensions.matches(Noun ~ Comitative ~ Plural, "velkaisuuksine")
  }

  "addDeclensions" should "handle case: rule: 41, word: viekas" in {
    val word = Word("viekas", 41, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "viekas")
    declensions.matches(Noun ~ Genitive ~ Singular, "viekkaan")
    declensions.matches(Noun ~ Partitive ~ Singular, "viekasta")

    declensions.matches(Noun ~ Illative ~ Singular, "viekkaaseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "viekkaalla")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "viekkaat")
    declensions.matches(Noun ~ Genitive ~ Plural, "viekkaiden", "viekkaitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "viekkaita")

    declensions.matches(Noun ~ Illative ~ Plural, "viekkaisiin", "viekkaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "viekkailla")

    declensions.matches(Noun ~ Essive ~ Plural, "viekkaina")

    declensions.matches(Noun ~ Instructive ~ Plural, "viekkain")
    declensions.matches(Noun ~ Comitative ~ Plural, "viekkaine")
  }

  "addDeclensions" should "handle case: rule: 42, word: mies" in {
    val word = Word("mies", 42, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "mies")
    declensions.matches(Noun ~ Genitive ~ Singular, "miehen")
    declensions.matches(Noun ~ Partitive ~ Singular, "miestä")

    declensions.matches(Noun ~ Illative ~ Singular, "mieheen")

    declensions.matches(Noun ~ Adessive ~ Singular, "miehellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "miehet")
    declensions.matches(Noun ~ Genitive ~ Plural, "miesten", "miehien")
    declensions.matches(Noun ~ Partitive ~ Plural, "miehiä")

    declensions.matches(Noun ~ Illative ~ Plural, "miehiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "miehillä")

    declensions.matches(Noun ~ Essive ~ Plural, "miehinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "miehin")
    declensions.matches(Noun ~ Comitative ~ Plural, "miehine")
  }

  "addDeclensions" should "handle case: rule: 43, word: kevyt" in {
    val word = Word("kevyt", 43, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kevyt")
    declensions.matches(Noun ~ Genitive ~ Singular, "kevyen")
    declensions.matches(Noun ~ Partitive ~ Singular, "kevyttä")

    declensions.matches(Noun ~ Illative ~ Singular, "kevyeen")

    declensions.matches(Noun ~ Adessive ~ Singular, "kevyellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kevyet")
    declensions.matches(Noun ~ Genitive ~ Plural, "kevyiden", "kevyitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "kevyitä")

    declensions.matches(Noun ~ Illative ~ Plural, "kevyihin", "kevyisiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kevyillä")

    declensions.matches(Noun ~ Essive ~ Plural, "kevyinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "kevyin")
    declensions.matches(Noun ~ Comitative ~ Plural, "kevyine")
  }

  "addDeclensions" should "handle case: rule: 44, word: kevät" in {
    val word = Word("kevät", 44, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kevät")
    declensions.matches(Noun ~ Genitive ~ Singular, "kevään")
    declensions.matches(Noun ~ Partitive ~ Singular, "kevättä")

    declensions.matches(Noun ~ Illative ~ Singular, "kevääseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "keväällä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "keväät")
    declensions.matches(Noun ~ Genitive ~ Plural, "keväiden", "keväitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "keväitä")

    declensions.matches(Noun ~ Illative ~ Plural, "keväisiin", "keväihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "keväillä")

    declensions.matches(Noun ~ Essive ~ Plural, "keväinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "keväin")
    declensions.matches(Noun ~ Comitative ~ Plural, "keväine")
  }

  "addDeclensions" should "handle case: rule: 45, word: kuudes" in {
    val word = Word("kuudes", 45, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kuudes")
    declensions.matches(Noun ~ Genitive ~ Singular, "kuudennen")
    declensions.matches(Noun ~ Partitive ~ Singular, "kuudetta")

    declensions.matches(Noun ~ Illative ~ Singular, "kuudenteen")

    declensions.matches(Noun ~ Adessive ~ Singular, "kuudennella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kuudennet")
    declensions.matches(Noun ~ Genitive ~ Plural, "kuudensien")
    declensions.matches(Noun ~ Partitive ~ Plural, "kuudensia")

    declensions.matches(Noun ~ Illative ~ Plural, "kuudensiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kuudensilla")

    declensions.matches(Noun ~ Essive ~ Plural, "kuudensina")

    declensions.matches(Noun ~ Instructive ~ Plural, "kuudensin")
    declensions.matches(Noun ~ Comitative ~ Plural, "kuudensine")
  }

  "addDeclensions" should "handle case: rule: 46, word: tuhat" in {
    val word = Word("tuhat", 46, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "tuhat")
    declensions.matches(Noun ~ Genitive ~ Singular, "tuhannen")
    declensions.matches(Noun ~ Partitive ~ Singular, "tuhatta")

    declensions.matches(Noun ~ Illative ~ Singular, "tuhanteen")

    declensions.matches(Noun ~ Adessive ~ Singular, "tuhannella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "tuhannet")
    declensions.matches(Noun ~ Genitive ~ Plural, "tuhansien", "tuhanten")
    declensions.matches(Noun ~ Partitive ~ Plural, "tuhansia")

    declensions.matches(Noun ~ Illative ~ Plural, "tuhansiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "tuhansilla")

    declensions.matches(Noun ~ Essive ~ Plural, "tuhansina")

    declensions.matches(Noun ~ Instructive ~ Plural, "tuhansin")
    declensions.matches(Noun ~ Comitative ~ Plural, "tuhansine")
  }

  "addDeclensions" should "handle case: rule: 47, word: uudestisyntynyt" in {
    val word = Word("uudestisyntynyt", 47, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "uudestisyntynyt")
    declensions.matches(Noun ~ Genitive ~ Singular, "uudestisyntyneen")
    declensions.matches(Noun ~ Partitive ~ Singular, "uudestisyntynyttä")

    declensions.matches(Noun ~ Illative ~ Singular, "uudestisyntyneeseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "uudestisyntyneellä")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "uudestisyntyneet")
    declensions.matches(Noun ~ Genitive ~ Plural, "uudestisyntyneiden", "uudestisyntyneitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "uudestisyntyneitä")

    declensions.matches(Noun ~ Illative ~ Plural, "uudestisyntyneisiin", "uudestisyntyneihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "uudestisyntyneillä")

    declensions.matches(Noun ~ Essive ~ Plural, "uudestisyntyneinä")

    declensions.matches(Noun ~ Instructive ~ Plural, "uudestisyntynein")
    declensions.matches(Noun ~ Comitative ~ Plural, "uudestisyntyneine")
  }


  "addDeclensions" should "handle case: rule: 48, word: vaate" in {
    val word = Word("vaate", 48, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vaate")
    declensions.matches(Noun ~ Genitive ~ Singular, "vaatteen")
    declensions.matches(Noun ~ Partitive ~ Singular, "vaatetta")

    declensions.matches(Noun ~ Illative ~ Singular, "vaatteeseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "vaatteella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "vaatteet")
    declensions.matches(Noun ~ Genitive ~ Plural, "vaatteiden", "vaatteitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "vaatteita")

    declensions.matches(Noun ~ Illative ~ Plural, "vaatteisiin", "vaatteihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vaatteilla")

    declensions.matches(Noun ~ Essive ~ Plural, "vaatteina")

    declensions.matches(Noun ~ Instructive ~ Plural, "vaattein")
    declensions.matches(Noun ~ Comitative ~ Plural, "vaatteine")
  }

  "addDeclensions" should "handle case: rule: 49, word: kannel" in {
    val word = Word("kannel", 49, Gradation("nt", "nn").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kannel")
    declensions.matches(Noun ~ Genitive ~ Singular, "kantelen")
    declensions.matches(Noun ~ Partitive ~ Singular, "kannelta")

    declensions.matches(Noun ~ Illative ~ Singular, "kanteleen")

    declensions.matches(Noun ~ Adessive ~ Singular, "kantelella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kantelet")
    declensions.matches(Noun ~ Genitive ~ Plural, "kantelien", "kannelten")
    declensions.matches(Noun ~ Partitive ~ Plural, "kantelia")

    declensions.matches(Noun ~ Illative ~ Plural, "kanteliin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kantelilla")

    declensions.matches(Noun ~ Essive ~ Plural, "kantelina")

    declensions.matches(Noun ~ Instructive ~ Plural, "kantelin")
    declensions.matches(Noun ~ Comitative ~ Plural, "kanteline")
  }

  "addDeclensions" should "handle case: rule: 49, word: kantele" in {
    val word = Word("kantele", 49, None)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kantele")
    declensions.matches(Noun ~ Genitive ~ Singular, "kanteleen")
    declensions.matches(Noun ~ Partitive ~ Singular, "kanteletta")

    declensions.matches(Noun ~ Illative ~ Singular, "kanteleeseen")

    declensions.matches(Noun ~ Adessive ~ Singular, "kanteleella")

    //Plural

    declensions.matches(Noun ~ Nominative ~ Plural, "kanteleet")
    declensions.matches(Noun ~ Genitive ~ Plural, "kanteleiden", "kanteleitten")
    declensions.matches(Noun ~ Partitive ~ Plural, "kanteleita")

    declensions.matches(Noun ~ Illative ~ Plural, "kanteleihin", "kanteleisiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kanteleilla")

    declensions.matches(Noun ~ Essive ~ Plural, "kanteleina")

    declensions.matches(Noun ~ Instructive ~ Plural, "kantelein")
    declensions.matches(Noun ~ Comitative ~ Plural, "kanteleine")
  }

}
