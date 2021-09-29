package morph_fin.utils

import org.scalatest._
import flatspec._
import matchers._


class HyphenationTest extends AnyFlatSpec with should.Matchers {

  "split - method" should "work" in {
    assert(Hyphenation.split("armeija") == Seq("ar", "mei", "ja"))
    assert(Hyphenation.split("koulu") == Seq("kou", "lu"))
    assert(Hyphenation.split("tietyö") == Seq("tie", "ty", "ö"))
    assert(Hyphenation.split("taika") == Seq("tai", "ka"))
    assert(Hyphenation.split("uudestaan") == Seq("uu", "des", "taan"))
    assert(Hyphenation.split("kanteleen") == Seq("kan", "te", "leen"))
    assert(Hyphenation.split("velkaisuus") == Seq("vel", "kai", "suus"))
    assert(Hyphenation.split("vaaan") == Seq("vaa", "an"))
    assert(Hyphenation.split("vaa^an") == Seq("vaa", "an"))
  }

  "addApostrophe - method" should "work" in {
    assert(Hyphenation.addApostrophes("kanteleen") == "kanteleen")
    assert(Hyphenation.addApostrophes("vaaan") == "vaa'an")
    assert(Hyphenation.addApostrophes("raaan") == "raa'an")
    assert(Hyphenation.addApostrophes("liuun") == "liu'un")
    assert(Hyphenation.addApostrophes("reiitin") == "rei'itin")
    assert(Hyphenation.addApostrophes("reiissä") == "rei'issä")
    assert(Hyphenation.addApostrophes("i^issä") == "i'issä")
    assert(Hyphenation.addApostrophes("ko^oissa") == "ko'oissa")
  }

}
