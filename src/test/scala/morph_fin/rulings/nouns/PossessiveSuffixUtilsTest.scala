package morph_fin.rulings.nouns

import org.scalatest.*
import flatspec.*
import matchers.*
import morph_fin.rulings.rules.{DeclensionRule, LoadAndParseNounRules}

class PossessiveSuffixUtilsTest extends AnyFlatSpec with should.Matchers {

  import PossessiveSuffixUtils._
  given Seq[DeclensionRule] = LoadAndParseNounRules.rules


  //TODO implement

  //kalleus, kaksi, kauniimman, kauniiksi
}
