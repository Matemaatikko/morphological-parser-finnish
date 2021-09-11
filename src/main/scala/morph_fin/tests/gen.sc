
import morph_fin.kotus_format.ReformatKotus

def timed[A](fun: => A): A = {
  val start = System.currentTimeMillis()
  val result = fun
  val end = System.currentTimeMillis()
  println(s"Timed: ${end - start} ms")
  result
}



import morph_fin.inflection.{GenerateInflectedWords, TargetFile}

//timed(GenerateInflectedWords.apply(TargetFile.Noun))

timed(ReformatKotus.reformat)