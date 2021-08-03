package morph_fin.utils

import org.scalatest._
import flatspec._
import matchers._


class HyphenationTest extends AnyFlatSpec with should.Matchers {

  "Hyphenation" should "work" in {
    assert(Hyphenation.apply("armeija") == Seq("ar", "mei", "ja"))
    assert(Hyphenation.apply("koulu") == Seq("kou", "lu"))
    assert(Hyphenation.apply("tietyö") == Seq("tie", "ty", "ö"))
    assert(Hyphenation.apply("taika") == Seq("tai", "ka"))
  }
}
