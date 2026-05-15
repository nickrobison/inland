package com.nickrobison.inland.allocator

import java.lang.foreign.{Arena, MemorySegment}
import scala.collection.mutable.ArrayBuffer



class ArenaAllocator(arena: Arena, slabSize: Long, slabs: ArrayBuffer[MemorySegment]) extends NativeAllocator {

  override def allocate[A: Layout](count: Long): MemorySegment = {
    val size = alignedSize(Layout[A].byteSize * count)
    arena.allocate(size)
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
}

object ArenaAllocator {
  def apply(size: Long)(using arena: Arena): ArenaAllocator = new ArenaAllocator(arena, size, ArrayBuffer.empty)
}
