package io.kaitai.struct

import scala.collection.mutable.ListBuffer
import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.{Endianness, FixedEndian, InheritedEndian}
import io.kaitai.struct.datatype.DataType.{KaitaiStreamType, UserTypeInstream}
  import io.kaitai.struct.format.{AttrSpec, ClassSpec, ClassSpecs, InstanceIdentifier, InstanceSpec, IoIdentifier, MemberSpec, ParentIdentifier, RootIdentifier}
import io.kaitai.struct.languages.CLispCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class CLispClassCompiler(
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, CLispCompiler) {

  // Getting the specific instance and subclass of the ClassCompiler
  // is based on code in NimClassCompiler
  val clisplang = lang.asInstanceOf[CLispCompiler]

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

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs += AttrSpec(List(), IoIdentifier, KaitaiStreamType)
    extraAttrs += AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None))
    extraAttrs += AttrSpec(List(), ParentIdentifier, curClass.parentType)

    extraAttrs ++= ExtraAttrs.forClassSpec(curClass, lang)

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    // Print out the class header
    lang.classHeader(curClass.name)

    // Compile the class attribute declarations
    compileAttrDeclarations(curClass.seq ++ extraAttrs)

    // Print out the class footer
    lang.classFooter(curClass.name)
  }

  /**
    * Iterates over a given list of attributes and generates attribute
    * declarations for each of them.
    * @param attrs attribute list to traverse
    */
  override def compileAttrDeclarations(attrs: List[MemberSpec]): Unit = {
    if (!attrs.isEmpty)
      clisplang.attributeDeclarationHeader()

    var first = true
    attrs.foreach { (attr) =>
      val isNilable = if (lang.switchBytesOnlyAsRaw) {
        attr.isNullableSwitchRaw
      } else {
        attr.isNullable
      }
      clisplang.attributeDeclarationPrefix(attr.id, first)
      lang.attributeDeclaration(attr.id, attr.dataTypeComposite, isNilable)
      first = false
    }

    if (!attrs.isEmpty)
      clisplang.attributeDeclarationFooter()
  }

  def compileReadFunction(curClass: ClassSpec) = {}
  override def compileInstances(curClass: ClassSpec) = {}
  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = ()

}
