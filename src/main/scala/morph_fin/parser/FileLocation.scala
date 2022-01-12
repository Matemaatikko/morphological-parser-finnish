package morph_fin.parser

import java.io.File

object FileLocation {
  val project_path = new File(FileLocation.getClass.getProtectionDomain().getCodeSource().getLocation().getPath()).getParentFile.getParentFile.getParentFile.getAbsolutePath
  val result_path = new File(project_path + "/files/result").getAbsolutePath
}
