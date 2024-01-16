package io.kaitai.struct

import io.kaitai.struct.format.{ClassSpec, ClassSpecs}
import io.kaitai.struct.languages.CLispCompiler

class CLispClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, CLispCompiler) {
}
