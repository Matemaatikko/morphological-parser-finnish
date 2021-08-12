package morph_fin.rulings

import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.nomines.Gradation

class GradationTest extends AnyFlatSpec with should.Matchers {

  import GradationHandler._

  "getWordGradationTypeForNomine - method" should "work" in {
    assert(getWordGradationTypeForNomine("nukke", Gradation("kk", "k")) == WordGradationType.Straight)
    assert(getWordGradationTypeForNomine("kannel", Gradation("kk", "k")) == WordGradationType.Inverted)
    assert(getWordGradationTypeForNomine("auto", Gradation("kk", "k")) == WordGradationType.Straight)
    assert(getWordGradationTypeForNomine("tuote", Gradation("kk", "k")) == WordGradationType.Inverted)
  }

  "getGradationTypeByEnding - method" should "work" in {
    assert(getGradationTypeByEnding("emme") == GradationType.Weak, "1")
    assert(getGradationTypeByEnding("aku") == GradationType.Strong, "2")
    assert(getGradationTypeByEnding("ien") == GradationType.Strong, "3")
    assert(getGradationTypeByEnding("aille") == GradationType.Weak, "4")
    assert(getGradationTypeByEnding("aalle") == GradationType.Strong, "5")
  }

  //TODO testing not comprehensive

  "resolveNomineException - method" should "work" in {
    val result1 = resolveNomineException("tuote", "elle", WordGradationType.Inverted, NomineMorphemes(Allative, Singular))
    assert(result1 == Some(GradationType.Strong))
  }


  "splitByGradationLocation - method" should "work ies" in {
    val gradation = Gradation("k", "")
    val result = splitByGradationLocation("ies", gradation)
    assert(result == ("i", "es"))
  }

  it should "work i-es" in {
    val gradation = Gradation("k", "")
    val result = splitByGradationLocation("i", gradation)
    assert(result == ("i", ""))
  }

  it should "work ike-en" in {
    val gradation = Gradation("k", "")
    val result = splitByGradationLocation("ike", gradation)
    assert(result == ("i", "e"))
  }

  it should "work lakka" in {
    val gradation = Gradation("kk", "k")
    val result = splitByGradationLocation("lakka", gradation)
    assert(result == ("la", "a"))
  }

  it should "work laka-n" in {
    val gradation = Gradation("kk", "k")
    val result = splitByGradationLocation("laka", gradation)
    assert(result == ("la", "a"))
  }

  it should "work tuote" in {
    val gradation = Gradation("tt", "t")
    val result = splitByGradationLocation("tuote", gradation)
    assert(result == ("tuo", "e"))
  }

  it should "work tuotteen" in {
    val gradation = Gradation("tt", "t")
    val result = splitByGradationLocation("tuotteen", gradation)
    assert(result == ("tuo", "een"))
  }

}
