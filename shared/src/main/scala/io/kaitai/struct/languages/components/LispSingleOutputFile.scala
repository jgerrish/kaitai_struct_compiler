package io.kaitai.struct.languages.components

import io.kaitai.struct.{ImportList, StringLanguageOutputWriter, Utils}
import io.kaitai.struct.LispStringLanguageOutputWriter
import io.kaitai.struct.format.ClassSpec

import scala.collection.mutable.ListBuffer

/**
  * Common trait for LISP-style languages that have one output file
  * per ClassSpec.
  *
  * This file is considered to be composed of:
  *
  * * a header
  * * imports list
  * * output body
  */
trait LispSingleOutputFile extends SingleOutputFile {
  override val outHeader = new LispStringLanguageOutputWriter(indent)
  override val out = new LispStringLanguageOutputWriter(indent)
}
