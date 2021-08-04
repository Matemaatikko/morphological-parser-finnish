package morph_fin.utils

import java.io.File

object FilesLocation {

  val project_path = new File(FilesLocation.getClass.getProtectionDomain().getCodeSource().getLocation().getPath()).getParentFile.getParentFile.getParentFile.getAbsolutePath
  val files_path = new File(project_path + "/src/main/files/rules").getAbsolutePath
}
