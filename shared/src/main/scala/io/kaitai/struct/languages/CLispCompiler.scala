package io.kaitai.struct.languages

import io.kaitai.struct.datatype.{DataType, Endianness, FixedEndian, KSError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrLikeSpec, AttrSpec, ClassSpec, DocSpec, Identifier, InstanceIdentifier, IoIdentifier, NamedIdentifier, NumberedIdentifier, ParamDefSpec, ParentIdentifier, ProcessExpr, RawIdentifier, RepeatSpec, RootIdentifier, SpecialIdentifier, TextRef, UrlRef}
import io.kaitai.struct.languages.components.{AllocateIOLocalVar, LanguageCompiler, LanguageCompilerStatic, LowerHyphenCaseClasses, NoNeedForFullClassPath, ObjectOrientedLanguage, LispSingleOutputFile, UniversalDoc}
import io.kaitai.struct.translators.{AbstractTranslator, CLispTranslator}
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class CLispCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    // with EveryReadIsExpression
    with LowerHyphenCaseClasses
    with AllocateIOLocalVar
    with NoNeedForFullClassPath
    with ObjectOrientedLanguage
    with LispSingleOutputFile
    with UniversalDoc {

  import CLispCompiler._

  // Members declared in io.kaitai.struct.languages.components.AllocateIOLocalVar
  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = ""

  // Members declared in io.kaitai.struct.languages.components.ExceptionNames
  override def ksErrorName(err: KSError): String = ""

  // Members declared in io.kaitai.struct.languages.components.ExtraAttrs
  // override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = ???

  // Members declared in io.kaitai.struct.languages.components.LanguageCompiler
  override def alignToByte(io: String): Unit = ()
  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = ()
  override def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = ()
  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = ()
  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = ()

  def attributeDeclarationHeader(): Unit = out.putOpenParen()
  def attributeDeclarationFooter(): Unit = out.putCloseParen()

  /**
    * Generates an attribute declaration prefix for a single attribute.
    * @param attrName the name of the attribute
    * @param first this is the first attribute in the attribute list
    */
  // This should probably be made generic in the LanguageOutputWriter
  // subclass
  def attributeDeclarationPrefix(attrName: Identifier, first: Boolean): Unit = {
    attrName match {
      case IoIdentifier | ParentIdentifier | RootIdentifier | IoIdentifier =>
        // just ignore it for now, like in RustCompiler
        return
      case _ => {
        if (!first) {
          out.puts
          out.puts(" ")
        }
      }
    }
  }

  /**
    * Generates an attribute declaration for a single attribute.
    * @param attrName the name of the attribute
    * @param attrType the type of the attribute
    * @param isNilable the attribute can be nil
    */
  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    attrName match {
      case ParentIdentifier | RootIdentifier | IoIdentifier =>
        // just ignore it for now, like in RustCompiler
        return
      case _ => {
        // TODO: If someone wants to add strong type checking to the
        // Common LISP compiler, this is one way of doing it.  You'll
        // need to add a type conversion function, like
        // kaitaiType2NativeType in the RustCompiler.
        // Feel free to contribute
        // out.puts(s"(${idToStr(attrName)} :type (${kaitaiTypeToNativeType({attrType)} * *) :initarg :${idToStr(attrName)} :initform 0)")

        // For now, we won't use types
        out.puts(s"(${idToStr(attrName)} :initarg :${idToStr(attrName)} :initform 0)")
      }
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = ()
  override def classConstructorFooter: Unit = ()
  override def condIfFooter(expr: Ast.expr): Unit = ()
  override def condIfHeader(expr: Ast.expr): Unit = ()
  override def condRepeatEosFooter: Unit = ()
  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = ()
  override def condRepeatExprFooter: Unit = ()
  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: Ast.expr): Unit = ()
  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit = ()
  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = ()
  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: Ast.expr): Unit = ()

  /**
    * Generates file header for a top-level type.
    *
    * This defines the package for the binary structure class and sets
    * up imports for the runtime.  The imports are added to the the
    * output by the results method in SingleOutputFile.  This method
    * calls outImports which is overridden by this compiler class.
    *
    * This method is part of the LanguageCompiler class.
    *
    * @param topClassName top-level name type in KS notation (lower underscore)
    */
  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s";; $headerComment")
    // There are a lot of style choices here that would be really nice
    // to parameterize.  For example, we want a blank line after the
    // header comments, but not after the class comments.
    // A language for doing this would be nifty.
    outHeader.puts
    outHeader.puts
    // There are a lot of uses of type2class instead of a more generic
    // solution that defines types of identifiers.
    // A metadata or grammar file that describes a language could be
    // used to reduce duplication.
    outHeader.puts(s"(defpackage ${type2class(topClassName)}")
    importList.add(config.clispPackage)
    // We write to out here, since the default results method in
    // SingleOutputFile concatenates outHeader, outImports and out
    out.putCloseParen()
    out.puts
  }

  // override def outHeader

  // Default indent string for each level of indent is two spaces
  override def indent: String = "  "


  override def instanceCalculate(instName: Identifier, dataType: DataType, value: Ast.expr): Unit = ()
  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = ()
  override def instanceFooter: Unit = ()
  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = ()
  override def normalIO: String = ""
  override def popPos(io: String): Unit = ()
  override def pushPos(io: String): Unit = ()
  override def readFooter(): Unit = ()
  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = ()
  // def results(topClass: ClassSpec): Map[String, String] = ???
  override def runRead(name: List[String]): Unit = ()
  override def runReadCalc(): Unit = ()
  override def seek(io: String, pos: Ast.expr): Unit = ()

  override val translator: CLispTranslator = new CLispTranslator(typeProvider, importList)

  // Defined in LowerHyphenCaseClasses
  // This is a trait that can be used in any LanguageCompiler subclass
  // and others to convert a type name to lisp-style-naming
  // The default naming type is lower_under_score
  //
  // override def type2class(className: String): String = ""

  override def useIO(ioEx: Ast.expr): String = ""

  // Members declared in io.kaitai.struct.languages.components.NoNeedForFullClassPath
  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = ()

  /**
    * Renders the class footer
    *
    * @param name the name of the class
    */
  override def classFooter(name: String): Unit = {
    out.dec
    out.putCloseParen()
    out.puts
  }

  /**
    * Renders the class header
    *
    * @param name the name of the class
    */
  override def classHeader(name: String): Unit = {
    out.puts(s"(defclass ${type2class(name)} (kaitai-struct)")
    out.inc
    out.puts
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = ()
  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = ()

  // Members declared in io.kaitai.struct.languages.components.ObjectOrientedLanguage

  /**
    * Renders identifier to a string, specifically for a given
    * language and settings. This usually includes things like
    * case and separator conversion and does *not* include things
    * like prepending "@" or "this." or "self." that might be
    * used to access private member.
    *
    * @param id identifier to render
    * @return identifier as string
    *
    * This implementation is based on the one in RustCompiler
    * It may get changed as more language features get added.
    */
  def idToStr(id: io.kaitai.struct.format.Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerHyphenCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerHyphenCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
    }
  }

  def localTemporaryName(id: io.kaitai.struct.format.Identifier): String = ""
  def privateMemberName(id: io.kaitai.struct.format.Identifier): String = ""
  def publicMemberName(id: io.kaitai.struct.format.Identifier): String = ""

  // Members declared in io.kaitai.struct.languages.components.SingleOutputFile
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.lisp"

  /**
    * Generates imports clauses in target language format
    *
    * This generates the :use statements to import every package in
    * the import list.  It's normally included in the file header.
    *
    * This method is part of the SingleOutputFile trait.  It is called
    * by the results method in that trait.
    *
    * @return import
    */
  override def outImports(topClass: ClassSpec) =
    "  (:use " + importList.toList.map((x) => s":$x").mkString(" ") + ")"

  // Members declared in io.kaitai.struct.languages.components.SwitchOps
  override def switchCaseEnd(): Unit = ()
  override def switchCaseStart(condition: Ast.expr): Unit = ()
  override def switchElseStart(): Unit = ()
  override def switchEnd(): Unit = ()
  override def switchStart(id: Identifier, on: Ast.expr): Unit = ()

  // Members declared in io.kaitai.struct.languages.components.ValidateOps
  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = ()

  // Members declared in io.kaitai.struct.languages.components.UniversalDoc
  /**
    * Output a documentation string for a language component.
    *
    * @param doc A DocSpec which contains the documentation that
    * should be converted to a docstring or comment.
    *
    * For LISP-style languages, we output two semi-colons with only
    * one empty line of padding before the comment.
    */
  override def universalDoc(doc: DocSpec): Unit = {
    out.puts

    doc.summary.foreach(summary => {
      out.putsLines(";; ", summary)
      out.puts
    })

    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(";; ", "@see \"" + text + "\"")
      case ref: UrlRef => {
        out.putsLines(";; ", s"@see ${ref.toAhref}")
      }
      out.puts
    }
  }
}

object CLispCompiler extends LanguageCompilerStatic
  with LowerHyphenCaseClasses {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CLispCompiler(tp, config)
}
