package io.kaitai.struct.languages.components

import io.kaitai.struct.Utils

// This is a trait that can be used in any LanguageCompiler subclass
// and others to convert a type name to lisp-style-naming
// The default naming type is lower_under_score
trait LowerHyphenCaseClasses {
  // A trait method that converts a lower_under_score name to
  // lower-hyphen-score
  def type2class(name: String) = Utils.lowerHyphenCase(name)
}
