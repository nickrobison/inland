package com.nickrobison.inland.allocator

import java.lang.foreign.MemorySegment

trait NativeAllocator extends AutoCloseable {

  def allocate[A: Layout](count: Long): MemorySegment

  def reallocate[A: Layout](old: MemorySegment, oldCount: Long, newCount: Long): MemorySegment

  def free(segment: MemorySegment): Unit

  protected def alignedSize(size: Long): Long = math.max(8, size)
}