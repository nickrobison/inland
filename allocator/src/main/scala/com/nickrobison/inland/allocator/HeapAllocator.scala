package com.nickrobison.inland.allocator

import java.lang.foreign.{Arena, MemorySegment}
import java.nio.ByteBuffer

class HeapAllocator extends NativeAllocator {

  override def allocate[A: Layout](count: Long): MemorySegment = {
    val size = alignedSize(Layout[A].byteSize * count)
    val buffer = ByteBuffer.allocate(size.toInt)
    MemorySegment.ofBuffer(buffer)
  }

  override def reallocate[A: Layout](old: MemorySegment, oldCount: Long, newCount: Long): MemorySegment = {
    val size = alignedSize(Layout[A].byteSize * newCount)
    val newSegment = MemorySegment.ofArray(new Array[Byte](size.toInt))
    MemorySegment.copy(old, 0, newSegment, 0, oldCount)
    newSegment
  }

  override def free(segment: MemorySegment): Unit = () // Noop

  override def close(): Unit = ()
}
