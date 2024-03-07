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
  */
class LispStringLanguageOutputWriter(indentStr: String) extends StringLanguageOutputWriter(indentStr) {
  override def puts(s: String): Unit = {
    sb.append(indentNow)
    sb.append(s)
  }
  override def puts: Unit = sb.append("\n")
}
