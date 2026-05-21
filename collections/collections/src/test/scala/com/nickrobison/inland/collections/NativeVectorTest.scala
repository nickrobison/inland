package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.HeapAllocator
import com.nickrobison.inland.allocator.instances.given
import com.nickrobison.inland.types.Layout
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.lang.foreign.ValueLayout
import scala.collection.mutable.ArrayBuffer

class NativeVectorTest extends AnyFunSuite, ScalaCheckPropertyChecks {

  // ── Helpers ──────────────────────────────────────────────────────

  private given HeapAllocator = HeapAllocator()
  private val SZ = 4 // Int byteSize
  private val DSZ = 8 // Double byteSize

  private def filledIntVector(n: Int)(using HeapAllocator): NativeVector[Int] = {
    val v = NativeVector[Int](n)
    (0 until n).foreach(v.addOne)
    v
  }

  private def expectedAllocSize[A: Layout](count: Int): Long = {
    val raw = Layout[A].byteSize * count
    math.max(8, raw)
  }

  private def assertCapacityAtLeast[A](v: NativeVector[A], neededBytes: Long): Unit = {
    val cap = v.storage.byteSize()
    assert(
      cap >= neededBytes,
      s"segment capacity $cap < needed $neededBytes for ${v.length} elements")
  }

  // ═══════════════════════════════════════════════════════════════════
  // 1 — Edge Cases: Zero Initial Size
  // ═══════════════════════════════════════════════════════════════════

  test("zero init: addOne works") {
    val v = NativeVector[Int](0)
    v.addOne(42)
    assert(v.head == 42)
    assert(v.length == 1)
  }

  test("zero init: multiple addOne works") {
    val v = NativeVector[Int](0)
    (0 until 10).foreach(v.addOne)
    assert(v.length == 10)
    (0 until 10).foreach(i => assert(v(i) == i))
  }

  test("zero init: Double vector addOne works") {
    val v = NativeVector[Double](0)
    (0 until 5).map(_.toDouble).foreach(v.addOne)
    assert(v.length == 5)
    (0 until 5).foreach(i => assert(v(i) == i.toDouble))
  }

  test("zero init: initial segment has at least 8 bytes") {
    val v = NativeVector[Int](0)
    assert(v.storage.byteSize() >= 8)
  }

  test("zero init: first addOne does NOT trigger unnecessary resize") {
    val v = NativeVector[Int](0)
    val capBefore = v.storage.byteSize()
    v.addOne(42)
    assert(
      v.storage.byteSize() == capBefore,
      s"first addOne changed capacity $capBefore → ${v.storage.byteSize()}")
  }

  test("zero init: sequential addOne fills initial segment then resizes") {
    val v = NativeVector[Int](0)
    val initCapacity = v.storage.byteSize() / Layout[Int].byteSize

    for (_ <- 0L until initCapacity) v.addOne(1)
    assert(v.length == initCapacity)
    val capAfterFill = v.storage.byteSize()

    v.addOne(2)
    assert(v.storage.byteSize() > capAfterFill, "should resize when exceeding zero-init capacity")
  }

  test("zero init: Double first addOne does not trigger unnecessary resize") {
    val v = NativeVector[Double](0)
    val capBefore = v.storage.byteSize()
    v.addOne(3.14)
    assert(v.storage.byteSize() == capBefore, "Double zero-init first addOne should not resize")
  }

  test("zero init: many elements after zero init read back correctly") {
    val v = NativeVector[Double](0)
    (0 until 20).map(_.toDouble).foreach(v.addOne)
    assert(v.length == 20)
    for (i <- 0 until 20) assert(v(i) == i.toDouble, s"Double mismatch at $i")
  }

  // ═══════════════════════════════════════════════════════════════════
  // 2 — Edge Cases: Empty / Single-Element
  // ═══════════════════════════════════════════════════════════════════

  test("new vector with zero initial size has length 0") {
    val v = NativeVector[Int](0)
    assert(v.length == 0)
  }

  test("remove single element from size-1 vector") {
    val v = filledIntVector(1)
    val removed = v.remove(0)
    assert(removed == 0)
    assert(v.length == 0)
  }

  test("clear on empty vector leaves length at 0") {
    val v = NativeVector[Int](16)
    v.clear()
    assert(v.length == 0)
  }

  test("iterator over empty vector yields nothing") {
    val v = NativeVector[Int](16)
    assert(v.iterator.toList.isEmpty)
  }

  // ═══════════════════════════════════════════════════════════════════
  // 3 — Error Handling: Negative Index
  // ═══════════════════════════════════════════════════════════════════

  test("negative index always throws for read regardless of length") {
    for (len <- Seq(0, 1, 5, 100)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException](v(-1))
    }
  }

  test("insert at negative index throws") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException](v.insert(-1, 99))
  }

  test("remove at negative index throws") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException](v.remove(-1))
  }

  test("apply at negative index throws") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException](v(-1))
  }

  test("update at negative index throws") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException](v.update(-1, 99))
  }

  // ═══════════════════════════════════════════════════════════════════
  // 4 — Error Handling: Bounds (idx == length, idx > length)
  // ═══════════════════════════════════════════════════════════════════

  test("apply at index == length throws") {
    for (len <- Seq(0, 1, 5, 10)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException](v(len), s"read at length=$len")
    }
  }

  test("update at index == length throws") {
    for (len <- Seq(0, 1, 5, 10)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException](v.update(len, 99), s"update at length=$len")
    }
  }

  test("insert at idx == length is valid for all lengths") {
    for (len <- Seq(0, 1, 5)) {
      val v = filledIntVector(len)
      v.insert(len, 99)
      assert(v.length == len + 1)
      assert(v(len) == 99)
    }
  }

  test("insert at idx > length throws") {
    for (len <- Seq(0, 1, 5)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException](v.insert(len + 1, 99))
    }
  }

  test("remove on empty vector throws") {
    val v = NativeVector[Int](16)
    intercept[IndexOutOfBoundsException](v.remove(0))
  }

  test("remove past end throws") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException](v.remove(5))
  }

  test("apply at index past length throws") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException](v(10))
  }

  test("update at index past length throws") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException](v.update(10, 99))
  }

  test("apply after clear throws") {
    val v = filledIntVector(3)
    v.clear()
    intercept[IndexOutOfBoundsException](v(0))
  }

  test("raw bytes beyond currentSize are not read by apply") {
    val v = NativeVector[Int](8)
    (0 until 3).foreach(v.addOne)
    intercept[IndexOutOfBoundsException](v(3))
    intercept[IndexOutOfBoundsException](v(4))
    intercept[IndexOutOfBoundsException](v(7))
  }

  // ═══════════════════════════════════════════════════════════════════
  // 5 — Iterator Contract
  // ═══════════════════════════════════════════════════════════════════

  test("iterator yields all elements in order") {
    val v = filledIntVector(5)
    assert(v.iterator.toList == List(0, 1, 2, 3, 4))
  }

  test("iterator hasNext after exhaustion returns false") {
    val v = filledIntVector(3)
    val it = v.iterator
    it.next(); it.next(); it.next()
    assert(!it.hasNext)
  }

  test("multiple iterators on same vector are independent") {
    val v = filledIntVector(5)
    val it1 = v.iterator
    val it2 = v.iterator
    it1.next()
    it1.next()
    assert(it2.next() == 0)
  }

  test("iterator over large vector (1000 elements)") {
    val v = NativeVector[Int](16)
    (0 until 1000).foreach(v.addOne)
    val elems = v.iterator.toList
    assert(elems.length == 1000)
    assert(elems.head == 0)
    assert(elems.last == 999)
  }

  test("iterator values exactly match raw segment reads") {
    val v = NativeVector[Int](8)
    (0 until 8).foreach(v.addOne)
    val raw = v.storage
    var idx = 0
    for (elem <- v.iterator) {
      val fromRaw = raw.get(ValueLayout.JAVA_INT, (idx * SZ).toLong)
      assert(elem == fromRaw, s"iterator $elem != raw $fromRaw at $idx")
      idx += 1
    }
    assert(idx == 8)
  }

  test("after remove, iterator matches raw segment") {
    val v = NativeVector[Int](8)
    (0 until 6).foreach(v.addOne)
    v.remove(1)
    v.remove(3)
    val raw = v.storage
    val expected = Seq(0, 2, 3, 5)
    var idx = 0
    for (elem <- v.iterator) {
      assert(elem == expected(idx), s"iterator ${elem} != expected ${expected(idx)} at $idx")
      val fromRaw = raw.get(ValueLayout.JAVA_INT, (idx * SZ).toLong)
      assert(elem == fromRaw, s"iterator $elem != raw $fromRaw at $idx")
      idx += 1
    }
    assert(idx == 4, s"iterator should yield 4 elements, got $idx")
  }

  // ═══════════════════════════════════════════════════════════════════
  // 6 — Raw Memory Verification
  // ═══════════════════════════════════════════════════════════════════

  test("raw: sequential addOne Int matches direct segment reads") {
    val v = NativeVector[Int](8)
    (0 until 8).foreach(v.addOne)
    val raw = v.storage
    for (i <- 0 until 8) {
      val fromVector = v(i)
      val fromRaw = raw.get(ValueLayout.JAVA_INT, (i * SZ).toLong)
      assert(fromRaw == fromVector, s"mismatch at $i: raw=$fromRaw vector=$fromVector")
      assert(fromRaw == i, s"wrong value at $i: expected $i got $fromRaw")
    }
  }

  test("raw: sequential addOne Double matches direct segment reads") {
    val v = NativeVector[Double](8)
    (0 until 8).map(_.toDouble).foreach(v.addOne)
    val raw = v.storage
    for (i <- 0 until 8) {
      val fromVector = v(i)
      val fromRaw = raw.get(ValueLayout.JAVA_DOUBLE, (i * DSZ).toLong)
      assert(fromRaw == fromVector, s"mismatch at $i")
      assert(fromRaw == i.toDouble, s"wrong value at $i")
    }
  }

  test("raw: addOne past capacity still correct after resize") {
    val v = NativeVector[Int](4)
    (0 until 20).foreach(v.addOne)
    val raw = v.storage
    for (i <- 0 until 20) {
      val fromVector = v(i)
      val fromRaw = raw.get(ValueLayout.JAVA_INT, (i * SZ).toLong)
      assert(fromRaw == fromVector, s"mismatch at $i")
      assert(fromRaw == i, s"wrong value at $i: expected $i got $fromRaw")
    }
  }

  test("raw: insert at front — bytes reflect shift") {
    val v = NativeVector[Int](8)
    v.addOne(1); v.addOne(2); v.addOne(3)
    v.insert(0, 99)
    val raw = v.storage
    assert(raw.get(ValueLayout.JAVA_INT, 0L) == 99)
    assert(raw.get(ValueLayout.JAVA_INT, SZ.toLong) == 1)
    assert(raw.get(ValueLayout.JAVA_INT, (2L * SZ)) == 2)
    assert(raw.get(ValueLayout.JAVA_INT, (3L * SZ)) == 3)
  }

  test("raw: insert in middle — bytes reflect shift") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)
    v.insert(2, 99)
    val raw = v.storage
    assert(raw.get(ValueLayout.JAVA_INT, 0L) == 0)
    assert(raw.get(ValueLayout.JAVA_INT, SZ.toLong) == 1)
    assert(raw.get(ValueLayout.JAVA_INT, (2L * SZ)) == 99)
    assert(raw.get(ValueLayout.JAVA_INT, (3L * SZ)) == 2)
    assert(raw.get(ValueLayout.JAVA_INT, (4L * SZ)) == 3)
    assert(raw.get(ValueLayout.JAVA_INT, (5L * SZ)) == 4)
  }

  test("raw: remove from front — bytes show compaction") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)
    v.remove(0)
    val raw = v.storage
    for (i <- 0 until 4) {
      val expected = i + 1
      assert(raw.get(ValueLayout.JAVA_INT, (i * SZ).toLong) == expected, s"element $i")
    }
  }

  test("raw: remove from middle — bytes show compaction") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)
    v.remove(2)
    val raw = v.storage
    assert(raw.get(ValueLayout.JAVA_INT, 0L) == 0)
    assert(raw.get(ValueLayout.JAVA_INT, SZ.toLong) == 1)
    assert(raw.get(ValueLayout.JAVA_INT, (2L * SZ)) == 3)
    assert(raw.get(ValueLayout.JAVA_INT, (3L * SZ)) == 4)
  }

  test("raw: clear leaves segment but length is 0") {
    val v = NativeVector[Int](8)
    (0 until 5).foreach(v.addOne)
    val segBefore = v.storage
    v.clear()
    assert(v.length == 0)
    assert(v.storage.eq(segBefore), "clear should reuse same segment")
    assert(v.storage.byteSize() == segBefore.byteSize(), "segment size unchanged on clear")
  }

  test("raw: update changes underlying bytes") {
    val v = NativeVector[Int](8)
    (0 until 4).foreach(v.addOne)
    v.update(1, 99)
    assert(v.storage.get(ValueLayout.JAVA_INT, SZ.toLong) == 99)
  }

  test("raw: values survive repeated resize cycles") {
    val v = NativeVector[Int](2)
    (0 until 200).foreach(v.addOne)
    val raw = v.storage
    for (i <- 0 until 200) {
      assert(raw.get(ValueLayout.JAVA_INT, (i * SZ).toLong) == i, s"mismatch at $i")
    }
  }

  test("raw: Long type direct segment reads") {
    val longLayout = ValueLayout.JAVA_LONG
    val v = NativeVector[Long](4)
    (0L until 10L).foreach(v.addOne)
    val raw = v.storage
    for (i <- 0 until 10) {
      assert(raw.get(longLayout, (i * 8L)) == i, s"Long mismatch at $i")
    }
  }

  test("raw: Float type direct segment reads") {
    val floatLayout = ValueLayout.JAVA_FLOAT
    val v = NativeVector[Float](4)
    (0 until 8).map(_.toFloat).foreach(v.addOne)
    val raw = v.storage
    for (i <- 0 until 8) {
      assert(raw.get(floatLayout, (i * 4L)) == i.toFloat, s"Float mismatch at $i")
    }
  }

  test("raw: interleaved add/remove/insert matches") {
    val v = NativeVector[Int](8)
    v.addOne(10); v.addOne(30)
    v.insert(1, 20)
    v.remove(2)
    v.addOne(40)
    v.insert(0, 5)
    val raw = v.storage
    assert(v.length == 4)
    assert(raw.get(ValueLayout.JAVA_INT, 0L) == 5)
    assert(raw.get(ValueLayout.JAVA_INT, SZ.toLong) == 10)
    assert(raw.get(ValueLayout.JAVA_INT, (2L * SZ)) == 20)
    assert(raw.get(ValueLayout.JAVA_INT, (3L * SZ)) == 40)
  }

  test("raw: repeated insert at front followed by reads") {
    val v = NativeVector[Int](4)
    v.insert(0, 2); v.insert(0, 1); v.insert(0, 0)
    val raw = v.storage
    for (i <- 0 until 3) {
      assert(raw.get(ValueLayout.JAVA_INT, (i * SZ).toLong) == i, s"front-insert: index $i")
    }
  }

  test("raw: remove-all-then-add retains correct layout") {
    val v = NativeVector[Int](4)
    (0 until 4).foreach(v.addOne)
    (0 until 4).foreach(_ => v.remove(0))
    (0 until 4).foreach(v.addOne)
    val raw = v.storage
    for (i <- 0 until 4) {
      assert(raw.get(ValueLayout.JAVA_INT, (i * SZ).toLong) == i, s"index $i")
    }
  }

  test("raw: multiple vectors from same allocator do not interfere") {
    val v1 = NativeVector[Int](8)
    val v2 = NativeVector[Int](8)
    (0 until 8).foreach(i => v1.addOne(i * 10))
    (0 until 8).foreach(i => v2.addOne(i * 100))
    for (i <- 0 until 8) {
      assert(
        v1.storage.get(ValueLayout.JAVA_INT, (i * SZ).toLong) == i * 10,
        s"v1 element $i corrupted")
      assert(
        v2.storage.get(ValueLayout.JAVA_INT, (i * SZ).toLong) == i * 100,
        s"v2 element $i corrupted")
    }
  }

  test("raw: no stale data after reallocation") {
    val v = NativeVector[Int](2)
    val expected = ArrayBuffer.empty[Int]
    for (i <- 0 until 10) { v.addOne(i); expected += i }
    for (_ <- 0 until 4) { expected.remove(0); v.remove(0) }
    for (i <- 10 until 16) { v.addOne(i); expected += i }
    assert(v.length == expected.length)
    for (i <- 0 until v.length) {
      val fromRaw = v.storage.get(ValueLayout.JAVA_INT, (i * SZ).toLong)
      assert(fromRaw == expected(i), s"index $i: raw=$fromRaw expected=${expected(i)}")
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // 7 — Allocation / Growth Behavior
  // ═══════════════════════════════════════════════════════════════════

  test("growth: addOne past initial capacity grows and preserves data") {
    val v = NativeVector[Int](2)
    (0 until 100).foreach(v.addOne)
    assert(v.length == 100)
    for (i <- 0 until 100) assert(v(i) == i, s"mismatch at $i")
  }

  test("growth: grow after removing many elements") {
    val v = NativeVector[Int](16)
    (0 until 100).foreach(v.addOne)
    (0 until 80).foreach(_ => v.remove(0))
    assert(v.length == 20)
    assert(v.head == 80)
    assert(v(19) == 99)
    (0 until 50).foreach(v.addOne)
    assert(v.length == 70)
    assert(v.head == 80)
    assert(v(69) == 49)
  }

  test("growth: addOne exactly fills and one past triggers resize") {
    val initSize = 4
    val v = NativeVector[Int](initSize)
    (0 until initSize).foreach(v.addOne)
    assert(v.length == initSize)
    (0 until initSize).foreach(i => assert(v(i) == i))
    v.addOne(99)
    assert(v.length == initSize + 1)
    assert(v(initSize) == 99)
    (0 until initSize).foreach(i => assert(v(i) == i))
  }

  test("growth: repeated resize cycles preserve data") {
    val v = NativeVector[Int](2)
    (0 until 256).foreach(v.addOne)
    assert(v.length == 256)
    (0 until 256).foreach(i => assert(v(i) == i))
  }

  test("growth: initial segment has expected size") {
    for (initSize <- Seq(1, 2, 4, 8, 16, 64)) {
      val v = NativeVector[Int](initSize)
      assert(
        v.storage.byteSize() == expectedAllocSize[Int](initSize),
        s"initSize=$initSize: expected ${expectedAllocSize[Int](initSize)}, got ${v.storage.byteSize()}"
      )
    }
  }

  test("growth: segment never smaller than required for current elements") {
    val v = NativeVector[Int](4)
    for (_ <- 0 until 100) {
      assertCapacityAtLeast(v, v.length.toLong * Layout[Int].byteSize)
      v.addOne(0)
    }
  }

  test("growth: segment grows only when necessary (not on every addOne)") {
    val v = NativeVector[Int](16)
    val initialCap = v.storage.byteSize()
    for (_ <- 0 until 15) {
      v.addOne(0)
      assert(v.storage.byteSize() == initialCap, "segment grew before reaching capacity")
    }
    v.addOne(0)
    assert(v.storage.byteSize() == initialCap, "segment grew when count == capacity")
    v.addOne(0)
    assert(v.storage.byteSize() > initialCap, "segment should grow past initial capacity")
  }

  test("growth: segment size follows geometric growth during sequential add") {
    val v = NativeVector[Int](4)
    val expectedCaps = Seq(
      4 -> expectedAllocSize[Int](4),
      5 -> expectedAllocSize[Int](8),
      9 -> expectedAllocSize[Int](16),
      17 -> expectedAllocSize[Int](32),
      33 -> expectedAllocSize[Int](64),
      65 -> expectedAllocSize[Int](128)
    )
    for ((n, expectedCap) <- expectedCaps) {
      while (v.length < n) v.addOne(0)
      assert(
        v.storage.byteSize() == expectedCap,
        s"after $n elements: expected $expectedCap, got ${v.storage.byteSize()}")
    }
  }

  test("growth: add-remove-add cycle does not cause unbounded growth") {
    val v = NativeVector[Int](8)
    (0 until 16).foreach(v.addOne)
    val capAfterPhase1 = v.storage.byteSize()
    (0 until 15).foreach(_ => v.remove(0))
    assert(v.storage.byteSize() == capAfterPhase1, "segment should not shrink after removes")
    (0 until 5).foreach(v.addOne)
    assert(
      v.storage.byteSize() == capAfterPhase1,
      s"unnecessary growth: ${v.storage.byteSize()} > $capAfterPhase1")
  }

  test("growth: clear then refill does not add extra capacity") {
    val v = NativeVector[Int](8)
    (0 until 8).foreach(v.addOne)
    val capBefore = v.storage.byteSize()
    v.clear()
    (0 until 8).foreach(v.addOne)
    assert(v.storage.byteSize() == capBefore, "clear+refill should reuse same segment")
  }

  test("growth: invariant segment capacity >= length × elementSize") {
    val v = NativeVector[Int](4)
    for (_ <- 0 until 50) {
      v.addOne(0)
      assert(
        v.storage.byteSize() >= v.length * Layout[Int].byteSize,
        s"capacity ${v.storage.byteSize()} < needed ${v.length * Layout[Int].byteSize}")
    }
  }

  test("growth: remove all leaves length 0 but segment intact") {
    val v = NativeVector[Int](16)
    (0 until 10).foreach(v.addOne)
    val segBefore = v.storage
    (0 until 10).foreach(_ => v.remove(0))
    assert(v.length == 0)
    assert(v.storage.eq(segBefore), "segment should be reused after remove-all")
  }

  // ═══════════════════════════════════════════════════════════════════
  // 8 — Integration / Scenario
  // ═══════════════════════════════════════════════════════════════════

  test("mixed add/remove/insert sequence") {
    val v = NativeVector[Int](8)
    v.addOne(1); v.addOne(2); v.addOne(3)
    v.insert(0, 0)
    v.addOne(4)
    assert(v.length == 5)
    assert(v.toList == List(0, 1, 2, 3, 4))
  }

  test("remove all then add fresh elements") {
    val v = filledIntVector(5)
    (0 until 5).foreach(_ => v.remove(0))
    assert(v.length == 0)
    (0 until 5).foreach(v.addOne)
    assert(v.length == 5)
    (0 until 5).foreach(i => assert(v(i) == i))
  }

  test("remove from middle then insert at same position") {
    val v = filledIntVector(5)
    v.remove(2)
    v.insert(2, 99)
    assert(v.length == 5)
    assert(v.toList == List(0, 1, 99, 3, 4))
  }

  test("clear then fill with more elements than original") {
    val v = filledIntVector(3)
    v.clear()
    (0 until 10).foreach(v.addOne)
    assert(v.length == 10)
    (0 until 10).foreach(i => assert(v(i) == i))
  }

  test("large N (10000) forward and backward access") {
    val n = 10000
    val v = NativeVector[Int](16)
    (0 until n).foreach(v.addOne)
    for (i <- 0 until n) assert(v(i) == i, s"forward mismatch at $i")
    for (i <- (n - 1) to 0 by -1) assert(v(i) == i, s"backward mismatch at $i")
  }

  test("no stale data after remove") {
    val v = NativeVector[Int](16)
    v.addOne(10); v.addOne(20); v.addOne(30)
    v.remove(1)
    assert(v(1) == 30, "should be shifted value, not stale 20")
    assert(v.length == 2)
  }

  test("remove all elements one by one") {
    val v = filledIntVector(5)
    for (i <- 0 until 5) {
      val removed = v.remove(0)
      assert(removed == i, s"expected $i at step $i got $removed")
    }
    assert(v.length == 0)
  }

  test("addOne after many removes still works") {
    val v = NativeVector[Int](16)
    (0 until 10).foreach(v.addOne)
    (0 until 5).foreach(_ => v.remove(0))
    v.addOne(99)
    assert(v(v.length - 1) == 99)
  }

  test("clear then addOne works") {
    val v = filledIntVector(5)
    v.clear()
    v.addOne(99)
    assert(v.length == 1)
    assert(v.head == 99)
  }

  test("clear then insert works") {
    val v = filledIntVector(5)
    v.clear()
    v.insert(0, 10)
    v.insert(1, 20)
    assert(v.length == 2)
    assert(v.head == 10)
    assert(v(1) == 20)
  }

  test("insert interleaved with addOne preserves all elements") {
    val v = NativeVector[Int](16)
    v.addOne(10); v.addOne(30)
    v.insert(1, 20)
    assert(v.toList == List(10, 20, 30))
  }

  // ═══════════════════════════════════════════════════════════════════
  // 9 — Behavioral Parity with ArrayBuffer
  // ═══════════════════════════════════════════════════════════════════

  test("parity: addOne sequence matches ArrayBuffer") {
    forAll { (elements: List[Int]) =>
      val nv = NativeVector[Int](16)
      val ab = ArrayBuffer.empty[Int]
      elements.foreach { e =>
        nv.addOne(e); ab.addOne(e)
      }
      assert(nv.length == ab.length)
      assert(nv.iterator.toList == ab.iterator.toList)
    }
  }

  test("parity: insert at every position matches ArrayBuffer") {
    forAll { (init: List[Int], elem: Int) =>
      val nv = NativeVector[Int](16)
      val ab = ArrayBuffer.empty[Int]
      init.foreach { e =>
        nv.addOne(e); ab.addOne(e)
      }
      val positions = init.indices.toVector :+ init.length
      for (idx <- positions) { nv.insert(idx, elem); ab.insert(idx, elem) }
      assert(nv.length == ab.length)
      assert(nv.iterator.toList == ab.iterator.toList)
    }
  }

  test("parity: remove from every position matches ArrayBuffer") {
    forAll { (init: List[Int], pos: Int) =>
      val nv = NativeVector[Int](16)
      val ab = ArrayBuffer.empty[Int]
      init.foreach { e =>
        nv.addOne(e); ab.addOne(e)
      }
      whenever(init.nonEmpty) {
        val idx = (pos & 0x7fffffff) % init.length
        assert(nv.remove(idx) == ab.remove(idx))
        assert(nv.length == ab.length)
        assert(nv.iterator.toList == ab.iterator.toList)
      }
    }
  }

  test("parity: update at every position matches ArrayBuffer") {
    forAll { (init: List[Int], replacement: Int) =>
      whenever(init.nonEmpty) {
        val nv = NativeVector[Int](16)
        val ab = ArrayBuffer.empty[Int]
        init.foreach { e =>
          nv.addOne(e); ab.addOne(e)
        }
        for (idx <- init.indices) { nv(idx) = replacement; ab(idx) = replacement }
        assert(nv.length == ab.length)
        assert(nv.iterator.toList == ab.iterator.toList)
      }
    }
  }

  test("parity: mixed operations sequence matches ArrayBuffer") {
    forAll { (init: List[Int]) =>
      val nv = NativeVector[Int](16)
      val ab = ArrayBuffer.empty[Int]
      init.foreach { e =>
        nv.addOne(e); ab.addOne(e)
      }

      for (i <- nv.length until 2 * nv.length) {
        nv.addOne(i * 10)
        ab.addOne(i * 10)
      }
      assert(nv.iterator.toList == ab.iterator.toList)

      if (nv.length > 2) {
        for (_ <- Seq(0, 0, nv.length - 1)) {
          assert(nv.remove(0) == ab.remove(0))
        }
        assert(nv.iterator.toList == ab.iterator.toList)
      }

      nv.clear(); ab.clear()
      assert(nv.length == ab.length)

      (0 until 5).foreach { i =>
        nv.addOne(i); ab.addOne(i)
      }
      assert(nv.iterator.toList == ab.iterator.toList)
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // 10 — Contract
  // ═══════════════════════════════════════════════════════════════════

  test("contract: empty vector toString") {
    assert(NativeVector[Int](16).toString.nonEmpty)
  }

  test("contract: non-empty vector toString contains className") {
    val s = filledIntVector(3).toString
    assert(s.nonEmpty)
    assert(s.contains("NativeVector"))
  }

  test("contract: NativeVector extends AbstractBuffer") {
    assert(NativeVector[Int](16).isInstanceOf[scala.collection.mutable.AbstractBuffer[Int]])
  }

}
