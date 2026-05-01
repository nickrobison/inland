package com.nickrobison.inland.allocator

import java.lang.foreign.{MemoryLayout, MemorySegment, ValueLayout}

object instances {

  implicit object IntLayout extends Layout[Int] {

    // FIXME: Well, this is awful
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_INT_UNALIGNED

    private val handle = ValueLayout.JAVA_INT.varHandle()

    override def write(offset: Long, value: Int)(using segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_INT_UNALIGNED, offset, value)

    override def read(offset: Long)(using segment: MemorySegment): Int = segment.getAtIndex(ValueLayout.JAVA_INT_UNALIGNED, offset)
  }
}
