package com.nickrobison.inland.allocator

import java.lang.foreign.{MemoryLayout, MemorySegment}

trait Layout[A] {
  def memoryLayout: MemoryLayout

  def byteSize: Long = memoryLayout.byteSize()

  def write(offset: Long, value: A)(using segment: MemorySegment): Unit
  def read(offset: Long)(using segment: MemorySegment): A
}

object Layout {
  def apply[A](implicit ev: Layout[A]): Layout[A] = ev
}
