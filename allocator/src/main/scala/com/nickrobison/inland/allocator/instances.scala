package com.nickrobison.inland.allocator

import com.nickrobison.inland.types.Layout
import com.nickrobison.inland.types.Layout.specializedLayout

import java.lang.foreign.ValueLayout.OfDouble
import java.lang.foreign.{MemoryLayout, MemorySegment, ValueLayout}

object instances {

  given Layout[Int] = new Layout[Int] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_INT

    override def write(offset: Long, value: Int)(using segment: MemorySegment): Unit =
      segment.setAtIndex(ValueLayout.JAVA_INT, offset, value)

    override def read(offset: Long)(using segment: MemorySegment): Int = {
      segment.getAtIndex(ValueLayout.JAVA_INT, offset)
    }
  }

  given Layout[Double] = new Layout[Double] {

    override def memoryLayout: OfDouble = specializedLayout[Double]

    override def write(offset: Long, value: Double)(using segment: MemorySegment): Unit = {
      segment.setAtIndex(memoryLayout, offset, value)
    }

    override def read(offset: Long)(using segment: MemorySegment): Double =
      segment.getAtIndex(memoryLayout, offset)
  }

  given Layout[Long] = new Layout[Long] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_LONG

    override def write(offset: Long, value: Long)(using segment: MemorySegment): Unit =
      segment.setAtIndex(ValueLayout.JAVA_LONG, offset, value)

    override def read(offset: Long)(using segment: MemorySegment): Long =
      segment.getAtIndex(ValueLayout.JAVA_LONG, offset)
  }

  given Layout[Float] = new Layout[Float] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_FLOAT

    override def write(offset: Long, value: Float)(using segment: MemorySegment): Unit =
      segment.setAtIndex(ValueLayout.JAVA_FLOAT, offset, value)

    override def read(offset: Long)(using segment: MemorySegment): Float =
      segment.getAtIndex(ValueLayout.JAVA_FLOAT, offset)
  }

  given Layout[Byte] = new Layout[Byte] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_BYTE

    override def write(offset: Long, value: Byte)(using segment: MemorySegment): Unit =
      segment.setAtIndex(ValueLayout.JAVA_BYTE, offset, value)

    override def read(offset: Long)(using segment: MemorySegment): Byte =
      segment.getAtIndex(ValueLayout.JAVA_BYTE, offset)
  }

  given Layout[Char] = new Layout[Char] {
    override def memoryLayout: MemoryLayout = ValueLayout.JAVA_CHAR

    override def write(offset: Long, value: Char)(using segment: MemorySegment): Unit =
      segment.setAtIndex(ValueLayout.JAVA_CHAR, offset, value)

    override def read(offset: Long)(using segment: MemorySegment): Char =
      segment.getAtIndex(ValueLayout.JAVA_CHAR, offset)
  }
}
