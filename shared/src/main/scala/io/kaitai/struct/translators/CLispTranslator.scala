package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.datatype.DataType.EnumType
import io.kaitai.struct.exprlang.Ast

class CLispTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  // Members declared in io.kaitai.struct.translators.BaseTranslator
  def bytesToStr(value: String, encoding: String): String = ???
  def doEnumById(enumTypeAbs: List[String], id: String): String = ???
  def doEnumByLabel(enumTypeAbs: List[String], label: String): String = ???
  def doIfExp(condition: Ast.expr, ifTrue: Ast.expr, ifFalse: Ast.expr): String = ???
  def doName(s: String): String = ???

  // Members declared in io.kaitai.struct.translators.CommonMethods
  def arrayFirst(a: Ast.expr): String = ???
  def arrayLast(a: Ast.expr): String = ???
  def arrayMax(a: Ast.expr): String = ???
  def arrayMin(a: Ast.expr): String = ???
  def arraySize(a: Ast.expr): String = ???
  def arraySubscript(container: Ast.expr, idx: Ast.expr): String = ???
  def enumToInt(value: Ast.expr, et: EnumType): String = ???
  def floatToInt(value: Ast.expr): String = ???
  def intToStr(value: Ast.expr, num: Ast.expr): String = ???
  def strLength(s: Ast.expr): String = ???
  def strReverse(s: Ast.expr): String = ???
  def strSubstring(s: Ast.expr, from: Ast.expr, to: Ast.expr): String = ???
  def strToInt(s: Ast.expr, base: Ast.expr): String = ???
}
