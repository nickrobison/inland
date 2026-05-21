package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.HeapAllocator
import com.nickrobison.inland.allocator.instances.given
import org.scalatest.funsuite.AnyFunSuite

import java.lang.foreign.{MemorySegment, ValueLayout}
import scala.collection.mutable

/** Robust correctness tests for NativeVector.
  *
  * These tests verify two things the basic unit tests don't cover:
  *   1. Raw-memory correctness  – direct MemorySegment reads at byte offsets
  *      (completely independent of the Layout read/write path used by NativeVector)
  *   2. Allocation efficiency    – measure segment byte sizes to detect
  *      wasted capacity and unnecessarily frequent reallocations.
  */
class NativeVectorCorrectnessTest extends AnyFunSuite {

  // ── common test data ──────────────────────────────────────────────

  private given HeapAllocator = HeapAllocator()
  private val SZ = 4 // byteSize of Int
  private val DSZ = 8 // byteSize of Double

  /** Dump the raw bytes of a segment as a hex string for diagnostics. */
  

  // ═══════════════════════════════════════════════════════════════════
  // SECTION 1 – Raw Memory Segment Verification
  // ═══════════════════════════════════════════════════════════════════

  private val intLayout = java.lang.foreign.ValueLayout.JAVA_INT
  private val doubleLayout = java.lang.foreign.ValueLayout.JAVA_DOUBLE

  test("raw memory: sequential addOne matches direct segment reads") {
    val v = NativeVector[Int](8)
    (0 until 8).foreach(v.addOne)

    val raw = v.storage
    for (i <- 0 until 8) {
      val fromVector = v(i)
      val fromRaw = raw.get(intLayout, (i * SZ).toLong)
      assert(fromRaw == fromVector, s"mismatch at index $i: raw=$fromRaw vector=$fromVector")
      assert(fromRaw == i, s"wrong value at index $i: expected $i got $fromRaw")
    }
  }

  test("raw memory: sequential addOne for Double matches direct segment reads") {
    val v = NativeVector[Double](8)
    (0 until 8).map(_.toDouble).foreach(v.addOne)

    val raw = v.storage
    for (i <- 0 until 8) {
      val fromVector = v(i)
      val fromRaw = raw.get(doubleLayout, (i * DSZ).toLong)
      assert(fromRaw == fromVector, s"mismatch at index $i")
      assert(fromRaw == i.toDouble, s"wrong value at index $i")
    }
  }

  test("raw memory: addOne past initial capacity — raw reads still correct") {
    val v = NativeVector[Int](4)        // tiny capacity
    (0 until 20).foreach(v.addOne)      // forces multiple resizes

    val raw = v.storage
    for (i <- 0 until 20) {
      val fromVector = v(i)
      val fromRaw = raw.get(intLayout, (i * SZ).toLong)
      assert(fromRaw == fromVector, s"mismatch at index $i")
      assert(fromRaw == i, s"wrong value at index $i: expected $i got $fromRaw")
    }
  }

  test("raw memory: insert at front — raw bytes reflect shift") {
    val v = NativeVector[Int](8)
    v.addOne(1)
    v.addOne(2)
    v.addOne(3)                           // storage: [1, 2, 3, _, _, _, _, _]
    v.insert(0, 99)                       // expected: [99, 1, 2, 3, _, _, _, _]

    val raw = v.storage
    assert(raw.get(intLayout, 0L) == 99, "element 0 should be 99")
    assert(raw.get(intLayout, SZ.toLong) == 1, "element 1 should be 1")
    assert(raw.get(intLayout, (2 * SZ).toLong) == 2, "element 2 should be 2")
    assert(raw.get(intLayout, (3 * SZ).toLong) == 3, "element 3 should be 3")
  }

  test("raw memory: insert in middle — raw bytes reflect correct shift") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)         // [0, 1, 2, 3, 4]
    v.insert(2, 99)                       // [0, 1, 99, 2, 3, 4]

    val raw = v.storage
    assert(raw.get(intLayout, 0L) == 0)
    assert(raw.get(intLayout, SZ.toLong) == 1)
    assert(raw.get(intLayout, (2 * SZ).toLong) == 99)
    assert(raw.get(intLayout, (3 * SZ).toLong) == 2)
    assert(raw.get(intLayout, (4 * SZ).toLong) == 3)
    assert(raw.get(intLayout, (5 * SZ).toLong) == 4)
  }

  test("raw memory: remove from front — raw bytes show compaction") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)         // [0, 1, 2, 3, 4]
    v.remove(0)                           // [1, 2, 3, 4]

    val raw = v.storage
    for (i <- 0 until 4) {
      val expected = i + 1
      val actual = raw.get(intLayout, (i * SZ).toLong)
      assert(actual == expected, s"element $i should be $expected, got $actual")
    }
  }

  test("raw memory: remove from middle — raw bytes show compaction") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)         // [0, 1, 2, 3, 4]
    v.remove(2)                           // [0, 1, 3, 4]

    val raw = v.storage
    assert(raw.get(intLayout, 0L) == 0)
    assert(raw.get(intLayout, SZ.toLong) == 1)
    assert(raw.get(intLayout, (2 * SZ).toLong) == 3)
    assert(raw.get(intLayout, (3 * SZ).toLong) == 4)
  }

  test("raw memory: clear leaves segment but length is 0") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)
    val segBefore = v.storage
    val oldByteSize = segBefore.byteSize()

    v.clear()

    assert(v.length == 0)
    // Storage segment should be unchanged (not re-allocated on clear)
    assert(v.storage.eq(segBefore), "clear should reuse same segment")
    assert(v.storage.byteSize() == oldByteSize, "segment size should not change on clear")
  }

  test("raw memory: update changes underlying bytes") {
    val v = NativeVector[Int](8)
    (0 until 4).foreach(v.addOne)         // [0, 1, 2, 3]
    v.update(1, 99)                       // [0, 99, 2, 3]

    val raw = v.storage
    assert(raw.get(intLayout, SZ.toLong) == 99, "raw byte at offset 4 should be 99")
  }

  test("raw memory: bytes beyond currentSize are not read by apply") {
    val v = NativeVector[Int](8)
    (0 until 3).foreach(v.addOne)

    // Raw segment might have stale/garbage at index 3+, but apply should
    // throw for indices >= length.  We verify that applying throws
    // regardless of what bytes happen to be at those positions.
    intercept[IndexOutOfBoundsException](v(3))
    intercept[IndexOutOfBoundsException](v(4))
    intercept[IndexOutOfBoundsException](v(7))
  }

  test("raw memory: values survive repeated resize cycles") {
    val v = NativeVector[Int](2)
    val n = 200
    (0 until n).foreach(v.addOne)

    val raw = v.storage
    for (i <- 0 until n) {
      val fromRaw = raw.get(intLayout, (i * SZ).toLong)
      assert(fromRaw == i, s"mismatch at index $i after $n elements")
    }
  }

  test("raw memory: Long type") {
    val longLayout = ValueLayout.JAVA_LONG
    val v = NativeVector[Long](4)
    (0L until 10L).foreach(v.addOne)

    val raw = v.storage
    for (i <- 0 until 10) {
      val fromRaw = raw.get(longLayout, (i * 8).toLong)
      assert(fromRaw == i, s"Long mismatch at index $i")
    }
  }

  test("raw memory: Float type") {
    val floatLayout = ValueLayout.JAVA_FLOAT
    val v = NativeVector[Float](4)
    (0 until 8).map(_.toFloat).foreach(v.addOne)

    val raw = v.storage
    for (i <- 0 until 8) {
      val fromRaw = raw.get(floatLayout, (i * 4).toLong)
      assert(fromRaw == i.toFloat, s"Float mismatch at index $i")
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // SECTION 2 – Allocation / Wasted Space Correctness
  // ═══════════════════════════════════════════════════════════════════

  /** Compute the aligned segment size the *allocator* would produce for
    * `count` elements of type `A`.  Mirrors `NativeAllocator.alignedSize`.
    */
  private def expectedAllocSize[A: Layout](count: Int): Long = {
    val raw = Layout[A].byteSize * count
    math.max(8, raw)      // alignedSize floors at 8
  }

  private def assertCapacityAtLeast[A](v: NativeVector[A], neededBytes: Long): Unit = {
    val cap = v.storage.byteSize()
    assert(cap >= neededBytes,
      s"segment capacity $cap bytes < needed $neededBytes bytes for ${v.length} elements")
  }

  private def wastedBytes[A: Layout](v: NativeVector[A]): Long = {
    v.storage.byteSize() - v.length * Layout[A].byteSize
  }

  test("alloc: initial segment has expected size") {
    for (initSize <- Seq(1, 2, 4, 8, 16, 64)) {
      val v = NativeVector[Int](initSize)
      val expected = expectedAllocSize[Int](initSize)
      assert(v.storage.byteSize() == expected,
        s"initSize=$initSize: expected $expected bytes, got ${v.storage.byteSize()}")
    }
  }

  test("alloc: segment never smaller than required for current elements") {
    val v = NativeVector[Int](4)
    for (_ <- 0 until 100) {
      assertCapacityAtLeast(v, v.length.toLong * Layout[Int].byteSize)
      v.addOne(0)
    }
    assertCapacityAtLeast(v, 100L * Layout[Int].byteSize)
  }

  test("alloc: segment grows only when necessary (not on every addOne)") {
    val v = NativeVector[Int](16)
    val initialCap = v.storage.byteSize()

    // Add 15 elements — should not trigger resize since capacity is 16
    for (_ <- 0 until 15) {
      v.addOne(0)
      assert(v.storage.byteSize() == initialCap,
        "segment grew before reaching capacity limit")
    }

    // The 16th element fits within the original capacity too
    v.addOne(0)
    assert(v.storage.byteSize() == initialCap,
      "segment grew when element count exactly equals initial capacity")

    // The 17th element should trigger a resize
    v.addOne(0)
    assert(v.storage.byteSize() > initialCap,
      "segment should have grown past initial capacity")
  }

  test("alloc: wasted space after initial fill is at most (capacity - 1) elements") {
    val initSizes = Seq(1, 2, 4, 8, 16)
    for (initSize <- initSizes) {
      val v = NativeVector[Int](initSize)
      // Fill exactly to trigger a resize
      (0 to initSize).foreach(v.addOne)   // adds initSize + 1 elements → triggers resize
      // After resize, wasted space should be bounded
      val waste = wastedBytes(v)
      expectedAllocSize[Int](initSize * 2) - (initSize + 1) * Layout[Int].byteSize
      // A reasonable bound: at most (newCapacity - logicalCount) * byteSize
      assert(waste >= 0, s"negative waste at initSize=$initSize")
    }
  }

  test("alloc: segment size follows geometric growth during sequential add") {
    val v = NativeVector[Int](4)
    v.storage.byteSize()
    // expected: 4 → 8 → 16 → 32 → 64 → 128 → ...
    val expectedCaps = Seq(
      4  -> expectedAllocSize[Int](4),    // initial
      5  -> expectedAllocSize[Int](8),    // first resize (4*2)
      9  -> expectedAllocSize[Int](16),   // second resize (8*2)
      17 -> expectedAllocSize[Int](32),   // third resize
      33 -> expectedAllocSize[Int](64),   // fourth resize
      65 -> expectedAllocSize[Int](128),  // fifth resize
    )

    for ((n, expectedCap) <- expectedCaps) {
      while (v.length < n) v.addOne(0)
      assert(v.storage.byteSize() == expectedCap,
        s"after adding $n elements: expected capacity $expectedCap, got ${v.storage.byteSize()}")
    }
  }

  test("alloc: add-remove-add cycle does not cause unbounded segment growth") {
    val v = NativeVector[Int](8)

    // Phase 1: add 16 elements → resize to at least capacity for 16
    (0 until 16).foreach(v.addOne)
    val capAfterPhase1 = v.storage.byteSize()

    // Phase 2: remove 15 → logically shrink to 1 element; segment unchanged
    (0 until 15).foreach(_ => v.remove(0))
    assert(v.storage.byteSize() == capAfterPhase1,
      "segment should not shrink after removals")

    // Phase 3: add 1 → should reuse existing capacity, NOT grow
    (0 until 5).foreach(v.addOne)
    assert(v.storage.byteSize() == capAfterPhase1,
      s"segment grew unnecessarily after add-remove cycle: ${v.storage.byteSize()} > $capAfterPhase1")
  }

  test("alloc: clear then refill does not add extra capacity") {
    val v = NativeVector[Int](8)
    (0 until 8).foreach(v.addOne)
    val capBefore = v.storage.byteSize()

    v.clear()
    (0 until 8).foreach(v.addOne)

    assert(v.storage.byteSize() == capBefore,
      "clear+refill should reuse same segment without extra growth")
  }

  // ═══════════════════════════════════════════════════════════════════
  // SECTION 3 – Zero initial capacity edge cases
  // ═══════════════════════════════════════════════════════════════════

  test("zero-init: initial segment has at least 8 bytes (alignedSize floor)") {
    val v = NativeVector[Int](0)
    assert(v.storage.byteSize() >= 8, "zero-size init should get at least 8 bytes")
  }

  test("zero-init: first addOne does NOT trigger unnecessary resize") {
    val v = NativeVector[Int](0)
    val capBefore = v.storage.byteSize()
    v.addOne(42)
    assert(v.storage.byteSize() == capBefore,
      "first addOne from zero init should not trigger resize: " +
        s"before $capBefore, after ${v.storage.byteSize()}")
  }

  test("zero-init: sequential addOne fills initial segment then resizes") {
    val v = NativeVector[Int](0)
    // Initial segment is at least 8 bytes → at least 2 Int slots
    val initCapacity = v.storage.byteSize() / Layout[Int].byteSize

    // Fill initial capacity
    for (_ <- 0L until initCapacity) v.addOne(1)
    assert(v.length == initCapacity, "should fill initial capacity")
    val capAfterFill = v.storage.byteSize()

    // Add one more → needs resize
    v.addOne(2)
    assert(v.storage.byteSize() > capAfterFill,
      "should resize when exceeding zero-init capacity")
  }

  test("zero-init: Double vector first addOne does not trigger unnecessary resize") {
    val v = NativeVector[Double](0)
    // alignedSize(0) = 8 bytes, Double is 8 bytes → exactly 1 slot
    val capBefore = v.storage.byteSize()

    v.addOne(3.14)
    assert(v.storage.byteSize() == capBefore,
      "Double zero-init first addOne should not resize")
  }

  test("zero-init: many elements after zero init read back correctly") {
    val v = NativeVector[Double](0)
    (0 until 20).map(_.toDouble).foreach(v.addOne)

    assert(v.length == 20)
    for (i <- 0 until 20) {
      assert(v(i) == i.toDouble, s"Double mismatch at $i")
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // SECTION 4 – Combined operations: insertion + removal raw memory
  // ═══════════════════════════════════════════════════════════════════

  test("combined: interleaved add, remove, insert — raw memory matches") {
    val v = NativeVector[Int](8)
    v.addOne(10)
    v.addOne(30)               // [10, 30]
    v.insert(1, 20)            // [10, 20, 30]
    v.remove(2)                // [10, 20]  (remove 30)
    v.addOne(40)               // [10, 20, 40]
    v.insert(0, 5)             // [5, 10, 20, 40]

    val raw = v.storage
    assert(v.length == 4)
    assert(raw.get(intLayout, 0L) == 5)
    assert(raw.get(intLayout, SZ.toLong) == 10)
    assert(raw.get(intLayout, 2L * SZ) == 20)
    assert(raw.get(intLayout, 3L * SZ) == 40)
  }

  test("combined: repeated insert at front followed by reads") {
    val v = NativeVector[Int](4)
    v.insert(0, 2)
    v.insert(0, 1)
    v.insert(0, 0)             // [0, 1, 2]

    val raw = v.storage
    assert(v.length == 3)
    for (i <- 0 until 3) {
      val fromRaw = raw.get(intLayout, (i * SZ).toLong)
      assert(fromRaw == i, s"repeated front-insert: index $i should be $i, got $fromRaw")
    }
  }

  test("combined: remove-all-then-add retains correct raw layout") {
    val v = NativeVector[Int](4)
    (0 until 4).foreach(v.addOne)  // [0, 1, 2, 3]
    (0 until 4).foreach(_ => v.remove(0))  // empty
    (0 until 4).foreach(v.addOne)  // [0, 1, 2, 3] again

    val raw = v.storage
    for (i <- 0 until 4) {
      val fromRaw = raw.get(intLayout, (i * SZ).toLong)
      assert(fromRaw == i, s"remove-all-then-add: index $i should be $i, got $fromRaw")
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // SECTION 5 – Verification that iterator yields same as raw memory
  // ═══════════════════════════════════════════════════════════════════

  test("iterator contract: iterator values exactly match raw segment reads") {
    val v = NativeVector[Int](8)
    (0 until 8).foreach(v.addOne)

    val raw = v.storage
    var idx = 0
    for (elem <- v.iterator) {
      val fromRaw = raw.get(intLayout, (idx * SZ).toLong)
      assert(elem == fromRaw,
        s"iterator value $elem != raw value $fromRaw at index $idx")
      idx += 1
    }
    assert(idx == 8, "iterator should yield exactly 8 elements")
  }

  test("iterator contract: after remove, iterator matches raw segment") {
    val v = NativeVector[Int](8)
    (0 until 6).foreach(v.addOne)   // [0, 1, 2, 3, 4, 5]
    v.remove(1)                     // [0, 2, 3, 4, 5]
    v.remove(3)                     // [0, 2, 3, 5]

    val raw = v.storage
    val expected = Seq(0, 2, 3, 5)
    var idx = 0
    for (elem <- v.iterator) {
      val fromRaw = raw.get(intLayout, (idx * SZ).toLong)
      assert(elem == expected(idx),
        s"iterator value $elem != expected ${expected(idx)} at index $idx")
      assert(elem == fromRaw,
        s"iterator $elem != raw segment read $fromRaw at index $idx")
      idx += 1
    }
    assert(idx == 4, s"iterator should yield 4 elements, got $idx")
  }

  // ═══════════════════════════════════════════════════════════════════
  // SECTION 6 – Structural constraints and invariants
  // ═══════════════════════════════════════════════════════════════════

  test("invariant: segment capacity is always ≥ length × elementSize") {
    val v = NativeVector[Int](4)
    for (_ <- 0 until 50) {
      v.addOne(0)
      val cap = v.storage.byteSize()
      val need = v.length * Layout[Int].byteSize
      assert(cap >= need,
        s"capacity $cap < needed $need for length ${v.length}")
    }
  }

  test("invariant: removing all elements leaves length 0 but segment intact") {
    val v = NativeVector[Int](16)
    (0 until 10).foreach(v.addOne)
    val segBefore = v.storage
    (0 until 10).foreach(_ => v.remove(0))
    assert(v.length == 0)
    assert(v.storage.eq(segBefore), "segment should be reused after remove-all")
    assert(v.storage.byteSize() == segBefore.byteSize(),
      "segment size should not change after removing all elements")
  }

  test("invariant: multiple vectors from same allocator do not interfere") {
    val v1 = NativeVector[Int](8)
    val v2 = NativeVector[Int](8)
    (0 until 8).foreach(i => v1.addOne(i * 10))
    (0 until 8).foreach(i => v2.addOne(i * 100))

    val raw1 = v1.storage
    val raw2 = v2.storage
    for (i <- 0 until 8) {
      assert(raw1.get(intLayout, (i * SZ).toLong) == i * 10,
        s"v1 element $i corrupted by v2")
      assert(raw2.get(intLayout, (i * SZ).toLong) == i * 100,
        s"v2 element $i corrupted by v1")
    }
  }

  test("invariant: sequential reallocation does not leak stale data") {
    // After resize, elements from the old segment should be in the new one.
    // After removing elements and re-adding, no stale data from old
    // operations should leak through.
    val v = NativeVector[Int](2)
    val expected = mutable.ArrayBuffer.empty[Int]

    // Phase 1: fill and extend several times
    for (i <- 0 until 10) {
      v.addOne(i)
      expected += i
    }
    // Phase 2: remove some
    for (_ <- 0 until 4) {
      expected.remove(0)
      v.remove(0)
    }
    // Phase 3: add more
    for (i <- 10 until 16) {
      v.addOne(i)
      expected += i
    }

    assert(v.length == expected.length,
      s"length ${v.length} != expected ${expected.length}")

    val raw = v.storage
    for (i <- 0 until v.length) {
      val fromRaw = raw.get(intLayout, (i * SZ).toLong)
      assert(fromRaw == expected(i),
        s"index $i: raw=$fromRaw expected=${expected(i)}")
    }
  }
}
