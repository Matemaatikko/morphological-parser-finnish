package morph_fin.rulings.nouns

import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNomineRules}

class PossessiveSuffixUtilsTest extends AnyFlatSpec with should.Matchers {

  import PossessiveSuffixUtils._
  given Seq[DeclensionRule] = LoadAndParseNomineRules.rules


  //TODO implement
}
