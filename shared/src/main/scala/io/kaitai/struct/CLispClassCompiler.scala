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

  /**
    * This is the main method on the class.
    * It gets called when the class is compiled.
    * This method calls the other methods to generate the class.
    *
    * There are no parameters, but the variable topClass should be defined in the
    * method scope.
    *
    * @return The result of the compilation
    */
  override def compile: CompileLog.SpecSuccess = {
    lang.fileHeader(topClass.nameAsStr)

    compileClass(topClass)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  /**
    * Compile a class definition.
    *
    * @param The class as a ClassSpec object
    *        name is a useful method on the class that returns the class name
    *        as a string.
    */
  override def compileClass(curClass: ClassSpec): Unit = {
    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    // Print out the class header
    lang.classHeader(curClass.name)

    // Print out the class footer
    lang.classFooter(curClass.name)
  }

  def compileReadFunction(curClass: ClassSpec) = {}
  override def compileInstances(curClass: ClassSpec) = {}
  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = ()

}
