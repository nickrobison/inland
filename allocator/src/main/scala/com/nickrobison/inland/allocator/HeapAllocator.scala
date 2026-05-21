package com.nickrobison.inland.allocator

import com.nickrobison.inland.types.Layout

import java.lang.foreign.MemorySegment
import java.nio.ByteBuffer

/**
 * Allocator backed by direct (off-heap) ByteBuffers.
 *
 * Despite the "heap" name, this uses `ByteBuffer.allocateDirect` to guarantee alignment so that
 * aligned `ValueLayout` access (e.g. `JAVA_INT` not `JAVA_INT_UNALIGNED`) is legal on every
 * segment. Each segment is independently allocated; there is no arena or pooling.
 */
class HeapAllocator extends NativeAllocator {

  override def allocate[A: Layout](count: Long): MemorySegment = {
    val size = alignedSize(Layout[A].byteSize * count)
    val buffer = ByteBuffer.allocateDirect(size.toInt)
    MemorySegment.ofBuffer(buffer)
  }

  override def reallocate[A: Layout](
      old: MemorySegment,
      oldCount: Long,
      newCount: Long): MemorySegment = {
    val size = alignedSize(Layout[A].byteSize * newCount)
    val buffer = ByteBuffer.allocateDirect(size.toInt)
    val newSegment = MemorySegment.ofBuffer(buffer)
    val elementsToCopy = oldCount.min(newCount)
    val bytesToCopy = (elementsToCopy * Layout[A].byteSize).min(old.byteSize()).toInt
    if (bytesToCopy > 0) {
      MemorySegment.copy(old, 0, newSegment, 0, bytesToCopy)
    }
    newSegment
  }

  override def free(segment: MemorySegment): Unit = () // Noop – GC handles cleanup

  override def close(): Unit = ()
}
