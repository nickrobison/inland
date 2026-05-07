package com.nickrobison.inland.allocator

import java.lang.foreign.{MemoryLayout, MemorySegment, ValueLayout}

object instances {

  given Layout[Int] = new Layout[Int] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_INT_UNALIGNED

    override def write(offset: Long, value: Int)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_INT_UNALIGNED, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Int = segment.getAtIndex(ValueLayout.JAVA_INT_UNALIGNED, offset)
  }

  given Layout[Double] = new Layout[Double] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_DOUBLE_UNALIGNED

    override def write(offset: Long, value: Double)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_DOUBLE_UNALIGNED, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Double = segment.getAtIndex(ValueLayout.JAVA_DOUBLE_UNALIGNED, offset)
  }

  given Layout[Long] = new Layout[Long] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_LONG_UNALIGNED

    override def write(offset: Long, value: Long)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_LONG_UNALIGNED, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Long = segment.getAtIndex(ValueLayout.JAVA_LONG_UNALIGNED, offset)
  }

  given Layout[Float] = new Layout[Float] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_FLOAT_UNALIGNED

    override def write(offset: Long, value: Float)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_FLOAT_UNALIGNED, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Float = segment.getAtIndex(ValueLayout.JAVA_FLOAT_UNALIGNED, offset)
  }

  given Layout[Byte] = new Layout[Byte] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_BYTE

    override def write(offset: Long, value: Byte)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_BYTE, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Byte = segment.getAtIndex(ValueLayout.JAVA_BYTE, offset)
  }

  given Layout[Char] = new Layout[Char] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_CHAR_UNALIGNED

    override def write(offset: Long, value: Char)(implicit segment: MemorySegment): Unit = segment.setAtIndex(ValueLayout.JAVA_CHAR_UNALIGNED, offset, value)

    override def read(offset: Long)(implicit segment: MemorySegment): Char = segment.getAtIndex(ValueLayout.JAVA_CHAR_UNALIGNED, offset)
  }
}
