package morph_fin.utils

object TryOrElse {

  def apply[A](fun: => A, default: => A): A =
    try
      fun
    catch
      case _ => default

}
