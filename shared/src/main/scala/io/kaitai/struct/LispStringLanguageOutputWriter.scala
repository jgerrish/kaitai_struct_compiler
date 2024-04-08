package io.kaitai.struct

/**
  * LispStringLanguageOutputWriter is an output writer for LISP-style
  * languages.
  *
  * The major difference is that it doesn't add a newline after a
  * normal puts.  This matches the LISP style of closing parenthesis
  * on the same line as the last statement of a block.
  *
  * For example, it makes it easier to output this:
  * (if (> 3 2)
  *   foo
  *   bar)
  * Instead of this:
  * (if (> 3 2)
  *   foo
  *   bar
  * )
  *
  * This probably still need some tweaking, it will change some as
  * more language features get implemented.  But there might be a
  * better way with some generic intermediate form or AST.
  */
class LispStringLanguageOutputWriter(indentStr: String) extends StringLanguageOutputWriter(indentStr) {
  override def inc: Unit = indentLevel += 1
  override def dec: Unit = indentLevel -= 1
  override def indentNow: String = indentStr * indentLevel

  override def puts(s: String): Unit = {
    sb.append(indentNow)
    sb.append(s)
  }

  /**
    * Insert an open parenthesis, respecting indent level and Common
    * LISP style.
    * I think this needs work.
    */
  def putOpenParen(): Unit = {
    sb.append(indentNow)
    sb.append("(")
  }

  /**
    * Insert a close parenthesis, respecting indent level and Common
    * LISP style.
    */
  def putCloseParen(): Unit = {
    sb.append(")")
  }

  override def puts: Unit = sb.append("\n")
}
