package io.kaitai.struct

import scala.collection.mutable.ListBuffer
import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.{Endianness, FixedEndian, InheritedEndian}
import io.kaitai.struct.datatype.DataType.{KaitaiStreamType, UserTypeInstream}
import io.kaitai.struct.format.{AttrSpec, ClassSpec, ClassSpecs, InstanceIdentifier, InstanceSpec, IoIdentifier, MemberSpec, ParentIdentifier, ParseInstanceSpec, RootIdentifier, ValueInstanceSpec}
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
    lang.fileFooter(topClass.nameAsStr)

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
    provider.nowClass = curClass

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
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstanceDeclaration(instName, instSpec)
    }

    // Print out the class footer
    lang.classFooter(curClass.name)

    // In Common LISP class methods are defined outside the scope of
    // the class.  That includes methods for instances of classes.
    // The first parameter of the method is an instance of that
    // class.
    compileReadFunction(curClass)

    curClass.toStringExpr.foreach(expr => lang.classToString(expr))

    compileInstances(curClass)

    // compileAttrReaders(curClass.seq ++ extraAttrs)

    // Recursive types
    compileSubclasses(curClass)
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

  def compileReadFunction(curClass: ClassSpec) = {
    // compile the constructor instance method, "initialize-instance"
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      curClass.params
    )
    // Compile the class constructor instance method,
    // "initialize-instance", footer
    // class instance methods in Common LISP are defined outside the class.
    lang.classConstructorFooter

    // FIXME
    val defEndian = curClass.meta.endian match {
      case Some(fe: FixedEndian) => Some(fe)
      case _ => None
    }
    // lang.readHeader(defEndian, false)

    // compileSeq(curClass.seq, defEndian)
    compileEagerRead(curClass.seq, curClass.meta.endian)
  }

  override def compileInstances(curClass: ClassSpec) = {
    lang.instanceDeclHeader(curClass.name)
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, curClass.meta.endian)
    }
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // FIXME: support calculated endianness

    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    lang.instanceCheckCacheAndReturn(instName, dataType)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case i: ParseInstanceSpec =>
        lang.attrParse(i, instName, None) // FIXME
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName, dataType)
    lang.instanceFooter
  }

}
