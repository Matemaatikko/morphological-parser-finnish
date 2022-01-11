import morph_fin.parser._

val init = Empty[Int]

val list = Seq(
  "testi" -> 1,
  "teko" -> 2,
  "tauti" -> 3,
  "tekijä" -> 4
)

val result = FSABuilder.build(list)
result.find("testi")
result.find("testo")
result.traverse("testiä")
result.traverse("tau")
result.traverse("tautiko")

