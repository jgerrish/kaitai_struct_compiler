package io.kaitai.struct.languages

import scala.collection.mutable.ListBuffer
import io.kaitai.struct.datatype.{DataType, Endianness, FixedEndian, InheritedEndian, KSError}
import io.kaitai.struct.datatype.DataType.{BytesEosType, BytesLimitType, BytesTerminatedType, BytesType, IntType, ReadableType, UserType, USER_TYPE_NO_PARENT}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrLikeSpec, AttrSpec, ClassSpec, DocSpec, Identifier, InstanceIdentifier, IoIdentifier, NamedIdentifier, NoRepeat, NumberedIdentifier, ParamDefSpec, ParentIdentifier, ProcessCustom, ProcessExpr, ProcessRotate, ProcessXor, ProcessZlib, RawIdentifier, RepeatExpr, RepeatSpec, RootIdentifier, SpecialIdentifier, TextRef, UrlRef}
import io.kaitai.struct.languages.components.{AllocateIOLocalVar, EveryReadIsExpression, LanguageCompiler, LanguageCompilerStatic, LowerHyphenCaseClasses, NoNeedForFullClassPath, ObjectOrientedLanguage, LispSingleOutputFile, StreamStructNames, UniversalDoc}
import io.kaitai.struct.translators.{AbstractTranslator, CLispTranslator}
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class CLispCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    // EveryReadIsExpression ensures reads are expressions, not just
    // statements
    with EveryReadIsExpression
    with LowerHyphenCaseClasses
    // With Common LISP we don't need to allocate the IO variable
    // But include this here until the rest is cleaned up
    // TODO: Fix this too
    with AllocateIOLocalVar
    with NoNeedForFullClassPath
    with ObjectOrientedLanguage
    with LispSingleOutputFile
    with UniversalDoc {

  import CLispCompiler._


  override val translator: CLispTranslator = new CLispTranslator(typeProvider, importList)

  // Members declared in
  // io.kaitai.struct.languages.components.EveryReadIsExpression
  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = ""
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = ()
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = ()
  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = ()


  /**
    * Handle a simple assignment expression
    * This sets the slot with the value of the expression.
    *
    * The slot-value function call could be abstracted out somewhere
    * either into privateMemberName or a method above that.
    *
    * @param id identifier of a member variable to set
    * @param expr the value to set the stream to
    * @return the result of the assignment
    */
  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    val name = privateMemberName(id)

    if (config.useAccessors) {
      out.puts(s"(setf ($name ks) $expr)")
    } else {
      out.puts(s"(setf (slot-value ks '$name) $expr)")
    }
  }

  // TODO: Implement this
  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    val expr = dataType match {
      case t: ReadableType =>
        s"(${kstreamName}:read-${Utils.lowerHyphenCase(t.apiCall(defEndian))} $io)"
      case blt: BytesLimitType =>
        s"(${kstreamName}:read-bytes $io (int (${expression(blt.size)})))"
      case _: BytesEosType =>
        s"(${kstreamName}:read-bytes-full $io)"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"(${kstreamName}:read-bytes-term $io $terminator $include $consume $eosError)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isExternal(typeProvider.nowClass)) {
          ""
        } else {
          // TODO: This will need to be revisited when we
          // start testing nested structures
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "nil"
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          // TODO: This will need to be revisited when we
          // start testing nested structures
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", self._is_le"
            case _ => ""
          }
          s", $parent, self._root$addEndian"
        }
        s"${userType2class(t)}($addParams$io$addArgs)"

      // default match to satisfy sbt compiler warnings
      // Feel free to contribute with the proper fix here
      // Throw a Scala exception?  Generate a Common LISP exception?
      // The code is running, it's generating classes.  It's a start.
      case _ => ""
    }

    if (assignType != dataType) {
      s"$expr"
      // s"${ksToNim(assignType)}($expr)"
    } else {
      s"$expr"
    }
  }

  // TODO: Remove or document
  def userType2class(t: UserType): String = {
    val name = t.classSpec.get.name
    val prefix = if (t.isExternal(typeProvider.nowClass)) {
      s"${name.head}."
    } else {
      ""
    }
    s"$prefix${types2class(name)}"
  }
  // TODO: Remove or document
  def types2class(name: List[String]): String = name.map(x => type2class(x)).mkString(".")


  // Members declared in io.kaitai.struct.languages.components.AllocateIOLocalVar

  /*
   * TODO Get io naming right
   * TODO: Add translator code
   * TODO: Differentiate betwen doName and doLocalName
   * TODO: Add why one was chosen over the other
   */
  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = ""

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[-1]"
    }
  }

  // Members declared in io.kaitai.struct.languages.components.ExceptionNames
  override def ksErrorName(err: KSError): String = ""

  // Members declared in io.kaitai.struct.languages.components.ExtraAttrs
  // override def extraAttrForIO(id: Identifier, rep: RepeatSpec): List[AttrSpec] = {
  //   ListBuffer[AttrSpec]().toList()
  // }

  // Members declared in io.kaitai.struct.languages.components.LanguageCompiler
  override def alignToByte(io: String): Unit = ()
  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = ()

  // override def attrParse(attr: AttrLikeSpec, id: Identifier, defEndian: Option[Endianness]): Unit = {
  //   out.puts("TEST 1")
  // }

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
    val name = idToStr(attrName)

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
        if (config.useAccessors) {
          // The version with accessors
          out.puts(s"($name :initarg :$name :initform nil :accessor $name)")
        } else {
          // The version without accessors
          out.puts(s"($name :initarg :$name :initform nil)")
        }
      }
    }
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = ()
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
    outHeader.puts
    importList.add(config.clispPackage)

    // Maybe these should not be explicitly prefixed as namespaces
    // before every function.
    // in-package is what changes that requirement
    importList.add(s"${kstreamName}")
    importList.add(s"${kstructName}")
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
  override def normalIO: String = s"(slot-value ks '${kstructName}::ks)"
  override def popPos(io: String): Unit = ()
  override def pushPos(io: String): Unit = ()

  override def readFooter(): Unit = {
    out.dec
    out.puts(")")
    out.dec
    out.puts(")")
    out.puts
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    // This pattern is based the readHeader code in PythonCompiler
    val suffix = endian match {
      case None => ""
      case Some(e) => s"_${e.toSuffix}"
    }
    out.puts
    // We may not need stream, parent and root.  The commit that adds
    // this gets the compiler-runtime interaction working.  If later
    // experiments with embedded structures show it's not necessary,
    // we can remove it.
    // out.puts(s"(defmethod kaitai-read$suffix ((ks ${kstructName}:${kstructName}) stream parent root)")
    out.puts(s"(defmethod kaitai-read$suffix ((ks ${types2class(typeProvider.nowClass.name)}) stream parent root)")
    out.inc
    out.puts("(progn")
    out.inc
  }

  // def results(topClass: ClassSpec): Map[String, String] = ???

  override def runRead(name: List[String]): Unit = {
    name.foreach{ (ksName) =>
      out.inc
      out.puts
      out.puts(s"(kaitai-read $ksName nil nil nil)")
      out.dec
    }
  }

  override def runReadCalc(): Unit = ()
  override def seek(io: String, pos: Ast.expr): Unit = ()

  // Defined in LowerHyphenCaseClasses
  // This is a trait that can be used in any LanguageCompiler subclass
  // and others to convert a type name to lisp-style-naming
  // The default naming type is lower_under_score
  //
  // override def type2class(className: String): String = ""

  override def useIO(ioEx: Ast.expr): String = ""

  // Members declared in io.kaitai.struct.languages.components.NoNeedForFullClassPath

  /**
    * Generate a constructor
    *
    * For Common LISP and CLOS, this is usually the generic function
    * initialize-instance.  This is called by make-instance.
    *
    * All we really want to do in here is call "read" so that the
    * class is Resource Acquisition Is Initialization (RAII) friendly.
    */
  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    // The :after makes initialize-instance act like a traditional
    // constructor.
    // If you don't include it, you're responsible for initializing
    // the slots based on :initarg and :initform options in the
    // defclass.  We just want to run an additional method after
    // the slots are initialized.
    out.puts(s"(defmethod initialize-instance :after ((ks ${type2class(name)}) &rest initargs)")
    out.puts("  (kaitai-read ks nil nil nil)")
  }

  /**
    * Generate the constructor footer
    *
    */
  override def classConstructorFooter: Unit = {
    out.dec
    out.puts(")")
    out.puts    
  }

  /**
    * Renders the class footer
    *
    * @param name the name of the class
    */
  override def classFooter(name: String): Unit = {
    out.dec
    out.putCloseParen()
    out.puts
    out.puts    
  }

  /**
    * Renders the class header
    *
    * @param name the name of the class
    */
  override def classHeader(name: String): Unit = {
    out.puts(s"(defclass ${type2class(name)} (${kstructName}:${kstructName})")
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

  /**
    * Return the name of a private member
    * This returns a string representation of a private member name
    *
    * In Common LISP, setting variables with setf on objects requires a slot-value
    * function call to access an object's slot.
    * This function doesn't call slot-value, perhaps it should.
    *
    * @param id identifier of a member variable to get the name for
    * @return the name of the private member variable
    */
  def privateMemberName(id: io.kaitai.struct.format.Identifier): String = {
    id match {
      case IoIdentifier => s"io"
      case RootIdentifier => s"root"
      case ParentIdentifier => s"parent"
      case _ => idToStr(id)
    }
  }

  def publicMemberName(id: io.kaitai.struct.format.Identifier): String = {
    idToStr(id)
  }

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
    with LowerHyphenCaseClasses
    with StreamStructNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new CLispCompiler(tp, config)

  // Members declared in io.kaitai.struct.languages.components.StreamStructNames
  override def kstreamName: String = "kaitai-stream"
  override def kstructName: String = "kaitai-struct"
}
