package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.{Layout, NativeAllocator}

import java.lang.foreign.MemorySegment
import scala.collection.mutable

class NativeVector[A: Layout] private(private var storage: MemorySegment, initialSize: Int)(implicit allocator: NativeAllocator) extends mutable.AbstractBuffer[A] {

  private var currentSize: Int = initialSize

  override def prepend(elem: A): NativeVector.this.type = {
    insert(0, elem)
    this
  }

  override def insert(idx: Int, elem: A): Unit = {
    checkWithinBounds(idx, idx)
    ensureSize(currentSize + 1)
    currentSize += 1
    this(idx) = elem
  }

  override def insertAll(idx: Int, elems: IterableOnce[A]): Unit = ???

  override def remove(idx: Int): A = {
    checkWithinBounds(idx, idx + 1)
    val res = this(idx)
    val srcOffset = (idx + 1) * Layout[A].byteSize
    val destOffset = (idx) * Layout[A].byteSize
    val bytesToCopy = (currentSize - (idx + 1)) * Layout[A].byteSize
    MemorySegment.copy(this.storage, srcOffset, this.storage, destOffset, bytesToCopy)
    reduceToSize(currentSize - 1)
    res
  }

  override def remove(idx: Int, count: Int): Unit = ???

  override def patchInPlace(from: Int, patch: IterableOnce[A], replaced: Int): NativeVector.this.type = ???

  override def addOne(elem: A): NativeVector.this.type = ???

  override def clear(): Unit = ???

  override def update(idx: Int, elem: A): Unit = ???

  override def apply(i: Int): A = {
    checkWithinBounds(i, i)
    val offset = Layout[A].byteSize * i
    Layout[A].read(offset)(using storage)
  }

  override def length: Int = currentSize

  override def iterator: Iterator[A] = ???

  // TODO: Implement
  private inline def checkWithinBounds(lo: Int, hi: Int): Unit = ()

  private inline def computeOffset(idx: Int): Long = {
    idx * Layout[A].byteSize
  }

  private def ensureSize(n: Long): Unit = {
    val segmentSize = storage.byteSize()
    val currentOffset = currentSize * Layout[A].byteSize
    val needed = Layout[A].byteSize * n
    if ((segmentSize - currentOffset) <= needed) {
      // We need to resize the array by a factor of 1.5
      this.storage = allocator.reallocate(storage, currentOffset, (segmentSize * 1.5).toLong)
    }
  }

  private def reduceToSize(n: Int): Unit = {
    // TODO: We probably need to zero out things here
    currentSize = n
  }
}
