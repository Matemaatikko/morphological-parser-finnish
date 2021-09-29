package morph_fin.rulings.nouns

import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.*
import morph_fin.rulings.morpheme.{Abessive, Ablative, Adessive, Allative, Comitative, Elative, Essive, Genitive, Illative, Inessive, Instructive, Morphemes, Nominative, Noun, Partitive, Plural, Singular, Translative}
import morph_fin.rulings.rules.{DeclensionRule, Gradation, LoadAndParseNomineRules}



class DeclensionUtilsGradationTest extends AnyFlatSpec with should.Matchers {


  import DeclensionUtils._
  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules

  extension(list: Seq[InflectedWord])
    def matches(morphemes: Morphemes, words: String*) =
      assert(list.filter(_.morphemes == morphemes).map(_.word.toString).toSet == words.toSet, morphemes)


  // GRADATION
  //===================================================================

  "addDeclensions" should "handle gradation: A, Strong, word: vadelmikko" in {
    val word = Word("vadelmikko", 4, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vadelmikko")
    declensions.matches(Noun ~ Genitive ~ Singular, "vadelmikon")
    declensions.matches(Noun ~ Partitive ~ Singular, "vadelmikkoa")

    declensions.matches(Noun ~ Illative ~ Singular, "vadelmikkoon")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "vadelmikkojen", "vadelmikoitten", "vadelmikoiden")
    declensions.matches(Noun ~ Illative ~ Plural, "vadelmikoihin", "vadelmikkoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vadelmikoilla")
  }

  "addDeclensions" should "handle gradation: A, Weak, word: vahakas" in {
    val word = Word("vahakas", 41, Gradation("kk", "k").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vahakas")
    declensions.matches(Noun ~ Genitive ~ Singular, "vahakkaan")
    declensions.matches(Noun ~ Partitive ~ Singular, "vahakasta")

    declensions.matches(Noun ~ Illative ~ Singular, "vahakkaaseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "vahakkaiden", "vahakkaitten")
    declensions.matches(Noun ~ Illative ~ Plural, "vahakkaisiin", "vahakkaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vahakkailla")
  }

  "addDeclensions" should "handle gradation: B, Strong, word: vaippa" in {
    val word = Word("vaippa", 9, Gradation("pp", "p").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vaippa")
    declensions.matches(Noun ~ Genitive ~ Singular, "vaipan")
    declensions.matches(Noun ~ Partitive ~ Singular, "vaippaa")

    declensions.matches(Noun ~ Illative ~ Singular, "vaippaan")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "vaippojen", "vaippain")
    declensions.matches(Noun ~ Illative ~ Plural, "vaippoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vaipoilla")
  }

  "addDeclensions" should "handle gradation: B, Weak, word: valpas" in {
    val word = Word("valpas", 41, Gradation("pp", "p").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "valpas")
    declensions.matches(Noun ~ Genitive ~ Singular, "valppaan")
    declensions.matches(Noun ~ Partitive ~ Singular, "valpasta")

    declensions.matches(Noun ~ Illative ~ Singular, "valppaaseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "valppaiden", "valppaitten")
    declensions.matches(Noun ~ Illative ~ Plural, "valppaisiin", "valppaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "valppailla")
  }

  "addDeclensions" should "handle gradation: C, Strong, word: valapatto" in {
    val word = Word("valapatto", 1, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "valapatto")
    declensions.matches(Noun ~ Genitive ~ Singular, "valapaton")
    declensions.matches(Noun ~ Partitive ~ Singular, "valapattoa")

    declensions.matches(Noun ~ Illative ~ Singular, "valapattoon")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "valapattojen")
    declensions.matches(Noun ~ Illative ~ Plural, "valapattoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "valapatoilla")
  }

  "addDeclensions" should "handle gradation: C, Weak, word: valkaisematon" in {
    val word = Word("valkaisematon", 34, Gradation("tt", "t").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "valkaisematon")
    declensions.matches(Noun ~ Genitive ~ Singular, "valkaisemattoman")
    declensions.matches(Noun ~ Partitive ~ Singular, "valkaisematonta")

    declensions.matches(Noun ~ Illative ~ Singular, "valkaisemattomaan")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "valkaisemattomien", "valkaisematonten")
    declensions.matches(Noun ~ Illative ~ Plural, "valkaisemattomiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "valkaisemattomilla")
  }

  "addDeclensions" should "handle gradation: D, Strong, word: valkaisematon" in {
    val word = Word("aika", 9, Gradation("k", "").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "aika")
    declensions.matches(Noun ~ Genitive ~ Singular, "ajan")
    declensions.matches(Noun ~ Partitive ~ Singular, "aikaa")

    declensions.matches(Noun ~ Illative ~ Singular, "aikaan")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "aikojen", "aikain")
    declensions.matches(Noun ~ Illative ~ Plural, "aikoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "ajoilla")
  }

  "addDeclensions" should "handle gradation: D, Weak, word: varas" in {
    val word = Word("varas", 41, Gradation("k", "").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "varas")
    declensions.matches(Noun ~ Genitive ~ Singular, "varkaan")
    declensions.matches(Noun ~ Partitive ~ Singular, "varasta")

    declensions.matches(Noun ~ Illative ~ Singular, "varkaaseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "varkaiden", "varkaitten")
    declensions.matches(Noun ~ Illative ~ Plural, "varkaisiin", "varkaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "varkailla")
  }

  "addDeclensions" should "handle gradation: E, Strong, word: varpu" in {
    val word = Word("varpu", 1, Gradation("p", "v").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "varpu")
    declensions.matches(Noun ~ Genitive ~ Singular, "varvun")
    declensions.matches(Noun ~ Partitive ~ Singular, "varpua")

    declensions.matches(Noun ~ Illative ~ Singular, "varpuun")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "varpujen")
    declensions.matches(Noun ~ Illative ~ Plural, "varpuihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "varvuilla")
  }

  "addDeclensions" should "handle gradation: E, Weak, word: varvas" in {
    val word = Word("varvas", 41, Gradation("p", "v").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "varvas")
    declensions.matches(Noun ~ Genitive ~ Singular, "varpaan")
    declensions.matches(Noun ~ Partitive ~ Singular, "varvasta")

    declensions.matches(Noun ~ Illative ~ Singular, "varpaaseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "varpaiden", "varpaitten")
    declensions.matches(Noun ~ Illative ~ Plural, "varpaisiin", "varpaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "varpailla")
  }

  "addDeclensions" should "handle gradation: F, Strong, word: vastaveto" in {
    val word = Word("vastaveto", 1, Gradation("t", "d").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "vastaveto")
    declensions.matches(Noun ~ Genitive ~ Singular, "vastavedon")
    declensions.matches(Noun ~ Partitive ~ Singular, "vastavetoa")

    declensions.matches(Noun ~ Illative ~ Singular, "vastavetoon")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "vastavetojen")
    declensions.matches(Noun ~ Illative ~ Plural, "vastavetoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "vastavedoilla")
  }

  "addDeclensions" should "handle gradation: F, Weak, word: viihde" in {
    val word = Word("viihde", 48, Gradation("t", "d").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "viihde")
    declensions.matches(Noun ~ Genitive ~ Singular, "viihteen")
    declensions.matches(Noun ~ Partitive ~ Singular, "viihdettä")

    declensions.matches(Noun ~ Illative ~ Singular, "viihteeseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "viihteiden", "viihteitten")
    declensions.matches(Noun ~ Illative ~ Plural, "viihteisiin", "viihteihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "viihteillä")
  }

  "addDeclensions" should "handle gradation: G, Strong, word: viikinki" in {
    val word = Word("viikinki", 5, Gradation("nk", "ng").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "viikinki")
    declensions.matches(Noun ~ Genitive ~ Singular, "viikingin")
    declensions.matches(Noun ~ Partitive ~ Singular, "viikinkiä")

    declensions.matches(Noun ~ Illative ~ Singular, "viikinkiin")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "viikinkien")
    declensions.matches(Noun ~ Illative ~ Plural, "viikinkeihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "viikingeillä")
  }

  "addDeclensions" should "handle gradation: G, Weak, word: villakangas" in {
    val word = Word("villakangas", 41, Gradation("nk", "ng").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "villakangas")
    declensions.matches(Noun ~ Genitive ~ Singular, "villakankaan")
    declensions.matches(Noun ~ Partitive ~ Singular, "villakangasta")

    declensions.matches(Noun ~ Illative ~ Singular, "villakankaaseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "villakankaiden", "villakankaitten")
    declensions.matches(Noun ~ Illative ~ Plural, "villakankaisiin", "villakankaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "villakankailla")
  }

  "addDeclensions" should "handle gradation: H, Strong, word: ylempi" in {
    val word = Word("ylempi", 16, Gradation("mp", "mm").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "ylempi")
    declensions.matches(Noun ~ Genitive ~ Singular, "ylemmän")
    declensions.matches(Noun ~ Partitive ~ Singular, "ylempää")

    declensions.matches(Noun ~ Illative ~ Singular, "ylempään")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "ylempien", "ylempäin")
    declensions.matches(Noun ~ Illative ~ Plural, "ylempiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "ylemmillä")
  }

  "addDeclensions" should "handle gradation: H, Weak, word: hammas" in {
    val word = Word("hammas", 41, Gradation("mp", "mm").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "hammas")
    declensions.matches(Noun ~ Genitive ~ Singular, "hampaan")
    declensions.matches(Noun ~ Partitive ~ Singular, "hammasta")

    declensions.matches(Noun ~ Illative ~ Singular, "hampaaseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "hampaiden", "hampaitten")
    declensions.matches(Noun ~ Illative ~ Plural, "hampaisiin", "hampaihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "hampailla")
  }

  "addDeclensions" should "handle gradation: I, Strong, word: huolto" in {
    val word = Word("huolto", 1, Gradation("lt", "ll").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "huolto")
    declensions.matches(Noun ~ Genitive ~ Singular, "huollon")
    declensions.matches(Noun ~ Partitive ~ Singular, "huoltoa")

    declensions.matches(Noun ~ Illative ~ Singular, "huoltoon")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "huoltojen")
    declensions.matches(Noun ~ Illative ~ Plural, "huoltoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "huolloilla")
  }

  "addDeclensions" should "handle gradation: I, Weak, word: helle" in {
    val word = Word("helle", 48, Gradation("lt", "ll").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "helle")
    declensions.matches(Noun ~ Genitive ~ Singular, "helteen")
    declensions.matches(Noun ~ Partitive ~ Singular, "hellettä")

    declensions.matches(Noun ~ Illative ~ Singular, "helteeseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "helteiden", "helteitten")
    declensions.matches(Noun ~ Illative ~ Plural, "helteisiin", "helteihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "helteillä")
  }

  "addDeclensions" should "handle gradation: J, Strong, word: hento" in {
    val word = Word("hento", 1, Gradation("nt", "nn").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "hento")
    declensions.matches(Noun ~ Genitive ~ Singular, "hennon")
    declensions.matches(Noun ~ Partitive ~ Singular, "hentoa")

    declensions.matches(Noun ~ Illative ~ Singular, "hentoon")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "hentojen")
    declensions.matches(Noun ~ Illative ~ Plural, "hentoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "hennoilla")
  }

  "addDeclensions" should "handle gradation: J, Weak, word: himmennin" in {
    val word = Word("himmennin", 33, Gradation("nt", "nn").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "himmennin")
    declensions.matches(Noun ~ Genitive ~ Singular, "himmentimen")
    declensions.matches(Noun ~ Partitive ~ Singular, "himmennintä")

    declensions.matches(Noun ~ Illative ~ Singular, "himmentimeen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "himmentimien", "himmenninten")
    declensions.matches(Noun ~ Illative ~ Plural, "himmentimiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "himmentimillä")
  }

  "addDeclensions" should "handle gradation: K, Strong, word: kaarto" in {
    val word = Word("kaarto", 1, Gradation("rt", "rr").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kaarto")
    declensions.matches(Noun ~ Genitive ~ Singular, "kaarron")
    declensions.matches(Noun ~ Partitive ~ Singular, "kaartoa")

    declensions.matches(Noun ~ Illative ~ Singular, "kaartoon")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "kaartojen")
    declensions.matches(Noun ~ Illative ~ Plural, "kaartoihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kaarroilla")
  }

  "addDeclensions" should "handle gradation: K, Weak, word: kierre" in {
    val word = Word("kierre", 48, Gradation("rt", "rr").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kierre")
    declensions.matches(Noun ~ Genitive ~ Singular, "kierteen")
    declensions.matches(Noun ~ Partitive ~ Singular, "kierrettä")

    declensions.matches(Noun ~ Illative ~ Singular, "kierteeseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "kierteiden", "kierteitten")
    declensions.matches(Noun ~ Illative ~ Plural, "kierteisiin", "kierteihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kierteillä")
  }

  "addDeclensions" should "handle gradation: L, Strong, word: kurki" in {
    val word = Word("kurki", 7, Gradation("k", "j").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "kurki")
    declensions.matches(Noun ~ Genitive ~ Singular, "kurjen")
    declensions.matches(Noun ~ Partitive ~ Singular, "kurkea")

    declensions.matches(Noun ~ Illative ~ Singular, "kurkeen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "kurkien")
    declensions.matches(Noun ~ Illative ~ Plural, "kurkiin")

    declensions.matches(Noun ~ Adessive ~ Plural, "kurjilla")
  }

  "addDeclensions" should "handle gradation: L, Weak, word: lahje" in {
    val word = Word("lahje", 48, Gradation("k", "j").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "lahje")
    declensions.matches(Noun ~ Genitive ~ Singular, "lahkeen")
    declensions.matches(Noun ~ Partitive ~ Singular, "lahjetta")

    declensions.matches(Noun ~ Illative ~ Singular, "lahkeeseen")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "lahkeiden", "lahkeitten")
    declensions.matches(Noun ~ Illative ~ Plural, "lahkeisiin", "lahkeihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "lahkeilla")
  }

  "addDeclensions" should "handle gradation: M, Strong, word: luku" in {
    val word = Word("luku", 1, Gradation("k", "v").opt)
    val declensions = generateDeclensions(word)

    //Singular

    declensions.matches(Noun ~ Nominative ~ Singular, "luku")
    declensions.matches(Noun ~ Genitive ~ Singular, "luvun")
    declensions.matches(Noun ~ Partitive ~ Singular, "lukua")

    declensions.matches(Noun ~ Illative ~ Singular, "lukuun")

    //Plural

    declensions.matches(Noun ~ Genitive ~ Plural, "lukujen")
    declensions.matches(Noun ~ Illative ~ Plural, "lukuihin")

    declensions.matches(Noun ~ Adessive ~ Plural, "luvuilla")
  }
}
