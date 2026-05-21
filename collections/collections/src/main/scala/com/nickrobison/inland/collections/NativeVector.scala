package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.{CommonErrors, NativeAllocator}
import com.nickrobison.inland.types.Layout

import java.lang.foreign.MemorySegment
import java.util.NoSuchElementException
import scala.collection.mutable

class NativeVector[A: Layout] private (
    private[collections] var storage: MemorySegment,
    initialSize: Int)(implicit allocator: NativeAllocator)
    extends mutable.AbstractBuffer[A] {

  private var currentSize: Int = 0
  private var capacity: Int = (storage.byteSize() / Layout[A].byteSize).toInt

  override def prepend(elem: A): NativeVector.this.type = {
    insert(0, elem)
    this
  }

  override def insert(idx: Int, elem: A): Unit = {
    checkWithinBounds(idx, idx)
    ensureSize(currentSize + 1)
    var i = currentSize
    while (i > idx) {
      Layout[A].write(i, Layout[A].read(i - 1)(using storage))(using storage)
      i -= 1
    }
    currentSize += 1
    this(idx) = elem
  }

  override def insertAll(idx: Int, elems: IterableOnce[A]): Unit = ???

  override def remove(idx: Int): A = {
    checkWithinBounds(idx, idx + 1)
    val res = this(idx)
    val srcOffset = (idx + 1) * Layout[A].byteSize
    val destOffset = idx * Layout[A].byteSize
    val bytesToCopy = (currentSize - (idx + 1)) * Layout[A].byteSize
    MemorySegment.copy(this.storage, srcOffset, this.storage, destOffset, bytesToCopy)
    reduceToSize(currentSize - 1)
    res
  }

  override def remove(idx: Int, count: Int): Unit = ???

  override def patchInPlace(
      from: Int,
      patch: IterableOnce[A],
      replaced: Int): NativeVector.this.type = ???

  override def addOne(elem: A): NativeVector.this.type = {
    insert(currentSize, elem)
    this
  }

  override def clear(): Unit = reduceToSize(0)

  override def update(idx: Int, elem: A): Unit = {
    checkWithinBounds(idx, idx + 1)
    Layout[A].write(idx, elem)(using storage)
  }

  override def apply(i: Int): A = {
    checkWithinBounds(i, i + 1)
    Layout[A].read(i)(using storage)
  }

  override def length: Int = currentSize

  override protected[this] def className: String = "NativeVector"

  override def iterator: Iterator[A] = new Iterator[A] {

    private var iterIdx = 0

    override def hasNext: Boolean = iterIdx < NativeVector.this.currentSize

    override def next(): A = {
      if (!hasNext) throw NoSuchElementException()
      val a = apply(iterIdx)
      iterIdx += 1
      a
    }
  }

  private inline def checkWithinBounds(lo: Int, hi: Int): Unit = {
    if (lo < 0) throw CommonErrors.indexOutOfBounds(index = lo, max = currentSize - 1)
    if (hi > currentSize) throw CommonErrors.indexOutOfBounds(index = hi - 1, max = currentSize - 1)
  }

  private inline def computeOffset(idx: Int): Long = {
    idx * Layout[A].byteSize
  }

  private def ensureSize(n: Int): Unit = {
    if (capacity < n) {
      val newCapacity = math.max(n, capacity * 2)
      this.storage = allocator.reallocate(storage, currentSize.toLong, newCapacity.toLong)
      this.capacity = (storage.byteSize() / Layout[A].byteSize).toInt
    }
  }

  private def reduceToSize(n: Int): Unit = {
    currentSize = n
  }
}

object NativeVector {

  /** Create a new NativeVector with the given initial capacity. */
  def apply[A: Layout](initialSize: Int)(implicit allocator: NativeAllocator): NativeVector[A] = {
    val storage = allocator.allocate[A](initialSize)
    new NativeVector[A](storage, initialSize)
  }
}
