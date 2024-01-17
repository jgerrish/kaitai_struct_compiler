package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.{Endianness, FixedEndian, InheritedEndian}
import io.kaitai.struct.format.{ClassSpec, ClassSpecs, InstanceIdentifier, InstanceSpec}
import io.kaitai.struct.languages.CLispCompiler

class CLispClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, CLispCompiler) {

  override def compile: CompileLog.SpecSuccess = {
    lang.fileHeader(topClass.nameAsStr)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  override def compileClass(curClass: ClassSpec): Unit = ()

  def compileReadFunction(curClass: ClassSpec) = {}
  override def compileInstances(curClass: ClassSpec) = {}
  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = ()

}
