package morph_fin.rulings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
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
    val result1 = resolveNomineException("tuote", "elle", WordGradationType.Inverted, NomineMorphemes(Case.Allative, GNumber.Singular))
    assert(result1 == Some(GradationType.Strong))
  }
}
