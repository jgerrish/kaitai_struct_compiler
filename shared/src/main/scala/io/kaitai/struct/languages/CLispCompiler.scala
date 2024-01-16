package io.kaitai.struct.languages

import io.kaitai.struct.datatype.{DataType, Endianness, FixedEndian, KSError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrLikeSpec, AttrSpec, ClassSpec, DocSpec, Identifier, InstanceIdentifier, ParamDefSpec, ProcessExpr, RepeatSpec}
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic, NoNeedForFullClassPath, UniversalDoc}
import io.kaitai.struct.translators.{AbstractTranslator, CLispTranslator}
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class CLispCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with NoNeedForFullClassPath
    with UniversalDoc {

  import CLispCompiler._

  // Members declared in io.kaitai.struct.languages.components.ExceptionNames
  def ksErrorName(err: KSError): String = ???

  // Members declared in io.kaitai.struct.languages.components.ExtraAttrs
  def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = ???

  // Members declared in io.kaitai.struct.languages.components.LanguageCompiler
  def alignToByte(io: String): Unit = ???
  def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = ???
  def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = ???
  def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = ???
  def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = ???
  def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = ???
  def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = ???
  def classConstructorFooter: Unit = ???
  def condIfFooter(expr: Ast.expr): Unit = ???
  def condIfHeader(expr: Ast.expr): Unit = ???
  def condRepeatEosFooter: Unit = ???
  def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = ???
  def condRepeatExprFooter: Unit = ???
  def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = ???
  def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = ???
  def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = ???
  def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = ???
  def fileHeader(topClassName: String): Unit = ???
  def indent: String = ???
  def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = ???
  def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = ???
  def instanceFooter: Unit = ???
  def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = ???
  def normalIO: String = ???
  def popPos(io: String): Unit = ???
  def pushPos(io: String): Unit = ???
  def readFooter(): Unit = ???
  def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = ???
  def results(topClass: ClassSpec): Map[String, String] = ???
  def runRead(name: List[String]): Unit = ???
  def runReadCalc(): Unit = ???
  def seek(io: String, pos: Ast.expr): Unit = ???
  val translator: AbstractTranslator = ???
  def type2class(className: String): String = ???
  def useIO(ioEx: Ast.expr): String = ???

  // Members declared in io.kaitai.struct.languages.components.NoNeedForFullClassPath
  def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = ???
  def classFooter(name: String): Unit = ???
  def classHeader(name: String): Unit = ???
  def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = ???
  def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = ???

  // Members declared in io.kaitai.struct.languages.components.SwitchOps
  def switchCaseEnd(): Unit = ???
  def switchCaseStart(condition: Ast.expr): Unit = ???
  def switchElseStart(): Unit = ???
  def switchEnd(): Unit = ???
  def switchStart(id: Identifier, on: Ast.expr): Unit = ???

  // Members declared in io.kaitai.struct.languages.components.ValidateOps
  def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = ???

  // Members declared in io.kaitai.struct.languages.components.UniversalDoc
  def universalDoc(doc: DocSpec): Unit = ???
}

object CLispCompiler extends LanguageCompilerStatic
{
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CLispCompiler(tp, config)
}
