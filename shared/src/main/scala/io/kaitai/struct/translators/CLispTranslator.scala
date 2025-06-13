package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType.EnumType
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.EnumSpec

class CLispTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  // Members declared in io.kaitai.struct.translators.BaseTranslator
  override def bytesToStr(value: String, encoding: String): String = ""
  override def doByteArrayLiteral(arr: Seq[Byte]): String = ""
  override def doEnumByLabel(enumSpec: EnumSpec, label: String): String = ""
  override def doEnumById(enumSpec: EnumSpec, id: String): String = ""
  override def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = ""
  override def doName(s: String): String = s

  // Members declared in io.kaitai.struct.translators.CommonMethods
  override def arrayFirst(a: Ast.expr): String = ""
  override def arrayLast(a: Ast.expr): String = ""
  override def arrayMax(a: Ast.expr): String = ""
  override def arrayMin(a: Ast.expr): String = ""
  override def arraySize(a: Ast.expr): String = ""
  override def arraySubscript(container: Ast.expr, idx: Ast.expr): String = ""
  override def enumToInt(value: Ast.expr, et: EnumType): String = ""
  override def floatToInt(value: Ast.expr): String = ""
  override def intToStr(value: Ast.expr): String = ""
  override def strLength(s: Ast.expr): String = ""
  override def strReverse(s: Ast.expr): String = ""
  override def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String = ""
  override def strToInt(s: Ast.expr, base: Ast.expr): String = ""
}
