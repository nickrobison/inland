package com.nickrobison.inland.allocator

import java.lang.foreign.{Arena, MemorySegment}
import scala.collection.mutable.ArrayBuffer



class ArenaAllocator(arena: Arena, slabSize: Long, slabs: ArrayBuffer[MemorySegment]) extends NativeAllocator {

  private var slabOffset: Long = slabSize

  override def allocate[A: Layout](count: Long): MemorySegment = {
    // Make sure we align to the correct size
    val size = alignedSize(Layout[A].byteSize * count)

    // Check to see if we have enough space in the existing slab
    if ((slabSize - slabOffset) > size) {
      val sliced = slabs.last.asSlice(slabOffset, size)
      // Bump the pointer
      slabOffset = slabOffset + size
      sliced
      // If not and the requested size is less then the max slab size, just allocate a new one
    } else if (size < slabSize) {
      openSlab(size)
      val sliced = slabs.last.asSlice(slabOffset, size)
      slabOffset = slabOffset + size
      sliced
    } else { // Jumbo, so just allocate directly, add to the slab array and open a new one
      val jumboSlab = arena.allocate(size)
      openSlab(0)
      jumboSlab
    }
  }

  override def reallocate[A: Layout](old: MemorySegment, oldCount: Long, newCount: Long): MemorySegment = {
    val newSegment = allocate(newCount)
    val copyBytes = math.min(oldCount, newCount) * Layout[A].byteSize

    MemorySegment.copy(old, 0L, newSegment, 0L, copyBytes)
    newSegment
  }

  override def free(segment: MemorySegment): Unit = {
    // No-op
  }

  override def close(): Unit = arena.close()

  private def alignUp(offset: Long, align: Long): Long = (offset + align - 1) & ~(align - 1)

  private def openSlab(minBytes: Long): Unit = {
    val size = math.max(minBytes, slabSize)
    val seg = arena.allocate(size)
    slabs += seg
    slabOffset = 0
  }
}

object ArenaAllocator {
  def apply(size: Long)(using arena: Arena): ArenaAllocator = new ArenaAllocator(arena, size, ArrayBuffer.empty)
}
