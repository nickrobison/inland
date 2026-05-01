package com.nickrobison.inland.allocator

import java.lang.foreign.{MemoryLayout, MemorySegment}

trait Layout[A] {
  def memoryLayout: MemoryLayout

  def byteSize: Long = memoryLayout.byteSize()

  def write(offset: Long, value: A)(implicit segment: MemorySegment): Unit
  def read(offset: Long)(implicit segment: MemorySegment): A
}

object Layout {
  def apply[A](implicit ev: Layout[A]): Layout[A] = ev
}
