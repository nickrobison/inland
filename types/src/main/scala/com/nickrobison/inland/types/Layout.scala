package com.nickrobison.inland.types

import Layout.specializedLayout

import java.lang.foreign.{MemoryLayout, MemorySegment, ValueLayout}
import scala.compiletime.{erasedValue, error}

trait Layout[A] {
  def memoryLayout: MemoryLayout

  def byteSize: Long = memoryLayout.byteSize()

  def write(offset: Long, value: A)(using segment: MemorySegment): Unit
  def read(offset: Long)(using segment: MemorySegment): A
}

object Layout {
  def apply[A](implicit ev: Layout[A]): Layout[A] = ev

  transparent inline def specializedLayout[A]: MemoryLayout = {
    inline erasedValue[A] match {
      case _: Double => ValueLayout.JAVA_DOUBLE
      case _ => error("Sorry, I can't")
    }
  }
}
