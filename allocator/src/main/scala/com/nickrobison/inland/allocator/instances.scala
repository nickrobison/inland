package com.nickrobison.inland.allocator

import java.lang.foreign.{MemoryLayout, MemorySegment, ValueLayout}

object instances {

  implicit object IntLayout extends Layout[Int] {

    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_INT

    override def write(offset: Long, value: Int)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_INT, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Int = segment.getAtIndex(ValueLayout.JAVA_INT, offset)
  }

  implicit object DoubleLayout extends Layout[Double] {

    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_DOUBLE_UNALIGNED

    override def write(offset: Long, value: Double)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_DOUBLE_UNALIGNED, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Double = segment.getAtIndex(ValueLayout.JAVA_DOUBLE_UNALIGNED, offset)
  }
}
