package io.kaitai.struct.translators

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format.{EnumSpec, Identifier}
import io.kaitai.struct.languages.PHPCompiler
import io.kaitai.struct.{RuntimeConfig, Utils}

class PHPTranslator(provider: TypeProvider, config: RuntimeConfig) extends BaseTranslator(provider)
    with MinSignedIntegers {
  override def doIntLiteral(n: BigInt): String = {
    super.doIntLiteral(if (n >= Long.MinValue && n <= Utils.MAX_UINT64) {
      n.toLong // output unsigned 64-bit integers as signed (otherwise we would get a float and
               // lose precision)
    } else {
      n
    })
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "\"" + Utils.hexEscapeByteArray(arr) + "\""
  override def doByteArrayNonLiteral(elts: Seq[Ast.expr]): String =
    s"pack('C*', ${elts.map(translate).mkString(", ")})"

  // https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.double
  override val asciiCharQuoteMap: Map[Char, String] = Map(
    '\t' -> "\\t",
    '\n' -> "\\n",
    '\r' -> "\\r",
    '"' -> "\\\"",
    '\\' -> "\\\\",

    // allowed and required to not trigger variable interpolation
    '$' -> "\\$",

    '\f' -> "\\f",
    '\u000b' -> "\\v",
    '\u001b' -> "\\e"
  )

  override def strLiteralUnicode(code: Char): String =
    "\\u{%x}".format(code.toInt)

  override def genericBinOp(left: Ast.expr, op: Ast.binaryop, right: Ast.expr, extPrec: Int) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Div) =>
        s"intval(${super.genericBinOp(left, op, right, 0)})"
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s"${PHPCompiler.kstreamName}::mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.genericBinOp(left, op, right, extPrec)
    }
  }

  override def anyField(value: expr, attrName: String): String =
    s"${translate(value)}->${doName(attrName)}"

  override def doLocalName(s: String) = {
    s match {
      case Identifier.ITERATOR => "$_"
      case Identifier.ITERATOR2 => "$_buf"
      case Identifier.INDEX => "$i"
      case _ => s"$$this->${doName(s)}"
    }
  }

  override def doName(s: String) = s"${Utils.lowerCamelCase(s)}()"

  override def doInternalName(id: Identifier): String =
    PHPCompiler.privateMemberName(id)

  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = {
    val enumClass = types2classAbs(enumSpec.name)
    s"$enumClass::${Utils.upperUnderscoreCase(label)}"
  }
  override def doEnumById(enumSpec: EnumSpec, id: String): String =
    // Just an integer, without any casts / resolutions - one would have to look up constants manually
    id

  override def arraySubscript(container: expr, idx: expr): String =
    s"${translate(container)}[${translate(idx)}]"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"(${translate(condition)} ? ${translate(ifTrue)} : ${translate(ifFalse)})"

  // Predefined methods of various types
  override def strConcat(left: expr, right: expr, extPrec: Int) =
    genericBinOpStr(left, Ast.operator.Add, ".", right, extPrec)

  override def strToInt(s: expr, base: expr): String =
    s"intval(${translate(s)}, ${translate(base)})"

  override def enumToInt(v: expr, et: EnumType): String =
    translate(v)

  override def boolToInt(v: expr): String =
    s"intval(${translate(v)})"

  override def floatToInt(v: expr): String =
    s"intval(${translate(v)})"

  override def intToStr(i: expr): String =
    s"strval(${translate(i)})"

  override def bytesToStr(bytesExpr: String, encoding: String): String =
    s"""${PHPCompiler.kstreamName}::bytesToStr($bytesExpr, ${doStringLiteral(encoding)})"""

  override def bytesLength(b: Ast.expr): String =
    s"strlen(${translate(b)})"
  override def bytesSubscript(container: Ast.expr, idx: Ast.expr): String =
    s"ord(${translate(container, METHOD_PRECEDENCE)}[${translate(idx)}])"
  override def bytesFirst(b: Ast.expr): String =
    s"ord(${translate(b, METHOD_PRECEDENCE)}[0])"
  override def bytesLast(b: Ast.expr): String =
    s"ord(${translate(b, METHOD_PRECEDENCE)}[${bytesLength(b)} - 1])"
  override def bytesMin(b: Ast.expr): String =
    s"${PHPCompiler.kstreamName}::byteArrayMin(${translate(b)})"
  override def bytesMax(b: Ast.expr): String =
    s"${PHPCompiler.kstreamName}::byteArrayMax(${translate(b)})"

  override def strLength(s: expr): String =
    s"strlen(${translate(s)})"
  override def strReverse(s: expr): String =
    s"strrev(${translate(s)})"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"${PHPCompiler.kstreamName}::substring(${translate(s)}, ${translate(from)}, ${translate(to)})"

  override def arrayFirst(a: expr): String =
    s"${translate(a)}[0]"
  override def arrayLast(a: expr): String = {
    // For huge debate on efficiency of PHP last element of array methods, see:
    // https://stackoverflow.com/a/41795859/487064
    val v = translate(a)
    s"$v[count($v) - 1]"
  }
  override def arraySize(a: expr): String =
    s"count(${translate(a)})"
  override def arrayMin(a: Ast.expr): String =
    s"min(${translate(a)})"
  override def arrayMax(a: Ast.expr): String =
    s"max(${translate(a)})"

  override def doInterpolatedStringLiteral(exprs: Seq[Ast.expr]): String =
    if (exprs.isEmpty) {
      doStringLiteral("")
    } else {
      exprs.map(anyToStr).mkString(" . ")
    }

  val namespaceRef = if (config.phpNamespace.isEmpty) {
    ""
  } else {
    "\\" + config.phpNamespace
  }

  def types2classAbs(names: List[String]) =
    names match {
      case List("kaitai_struct") => PHPCompiler.kstructName
      case _ =>
        namespaceRef + "\\" + PHPCompiler.types2classRel(names)
    }
}
