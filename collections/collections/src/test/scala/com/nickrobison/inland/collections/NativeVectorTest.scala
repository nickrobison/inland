package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.HeapAllocator
import com.nickrobison.inland.allocator.instances.given
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Try

class NativeVectorTest extends AnyFunSuite {

  // ── Helpers ──────────────────────────────────────────────────────

  private given HeapAllocator = HeapAllocator()

  /** Build a vector with known contents [0, 1, ..., n-1]. */
  private def filledIntVector(n: Int)(using HeapAllocator): NativeVector[Int] = {
    val v = NativeVector[Int](n)
    (0 until n).foreach(v.addOne)
    v
  }

  private def filledDoubleVector(n: Int)(using HeapAllocator): NativeVector[Double] = {
    val v = NativeVector[Double](n)
    (0 until n).map(_.toDouble).foreach(v.addOne)
    v
  }

  private def asList(v: NativeVector[?]): List[?] =
    v.iterator.toList

  // ── 1. Construction and Initial State ────────────────────────────

  test("new vector length is 0") {
    val v = NativeVector[Int](16)
    assert(v.length == 0)
  }

  test("new vector with various initial sizes") {
    for (size <- Seq(1, 2, 8, 16, 64, 128)) {
      val v = NativeVector[Int](size)
      assert(v.length == 0, s"initial size=$size")
    }
  }

  test("new vector with zero initial size") {
    // Should still construct and have length 0
    val v = NativeVector[Int](0)
    assert(v.length == 0)
  }

  // ── 2. addOne ────────────────────────────────────────────────────

  test("addOne then apply returns same element") {
    val v = NativeVector[Int](16)
    v.addOne(42)
    assert(v(0) == 42)
  }

  test("addOne increases length by 1") {
    val v = NativeVector[Int](16)
    val before = v.length
    v.addOne(7)
    assert(v.length == before + 1)
  }

  test("multiple addOne preserves insertion order") {
    val v = NativeVector[Int](16)
    val elems = List(10, 20, 30, 40, 50)
    elems.foreach(v.addOne)
    assert(v.length == elems.length)
    assert(v(0) == 10)
    assert(v(1) == 20)
    assert(v(2) == 30)
    assert(v(3) == 40)
    assert(v(4) == 50)
  }

  test("addOne past initial capacity grows and preserves data") {
    val v = NativeVector[Int](2)           // tiny capacity
    val n = 100
    (0 until n).foreach(v.addOne)
    assert(v.length == n)
    for (i <- 0 until n) {
      assert(v(i) == i, s"mismatch at index $i")
    }
  }

  test("addOne after many removes still works") {
    val v = NativeVector[Int](16)
    (0 until 10).foreach(v.addOne)
    (0 until 5).foreach(_ => v.remove(0))
    v.addOne(99)
    assert(v(v.length - 1) == 99)
  }

  // ── 3. insert (KNOWN BUG: insert does not shift right) ───────────

  test("insert at position 0 on empty vector") {
    val v = NativeVector[Int](16)
    v.insert(0, 99)
    assert(v.length == 1)
    assert(v(0) == 99)
  }

  test("insert at end equals addOne") {
    val v = NativeVector[Int](16)
    v.insert(0, 10)
    v.insert(1, 20)
    assert(v.length == 2)
    assert(v(0) == 10)
    assert(v(1) == 20)
  }

  test("insert at beginning shifts existing elements right") {
    // BUG #2: insert overwrites without shifting right
    // Expected: [99, 1, 2, 3]
    // Actual:   [99, 2, 3, garbage]
    val v = NativeVector[Int](16)
    v.addOne(1)
    v.addOne(2)
    v.addOne(3)
    v.insert(0, 99)
    assert(v.length == 4)
    assert(v(0) == 99, "head should be new element")
    assert(v(1) == 1, "existing element at idx=0 should shift to idx=1")
    assert(v(2) == 2, "existing element at idx=1 should shift to idx=2")
    assert(v(3) == 3, "existing element at idx=2 should shift to idx=3")
  }

  test("insert in middle shifts tail elements right") {
    // BUG #2
    // Vector: [0, 1, 2, 3, 4]
    // Insert 99 at idx=2
    // Expected: [0, 1, 99, 2, 3, 4]
    val v = filledIntVector(5)
    v.insert(2, 99)
    assert(v.length == 6)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 99)
    assert(v(3) == 2)
    assert(v(4) == 3)
    assert(v(5) == 4)
  }

  test("insert at last position works like append") {
    val v = filledIntVector(3)
    v.insert(3, 99)
    assert(v.length == 4)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 2)
    assert(v(3) == 99)
  }

  test("insert at end (idx == length) is valid") {
    val v = NativeVector[Int](4)
    v.insert(0, 10)
    v.insert(1, 20) // append after growth
    assert(v(0) == 10)
    assert(v(1) == 20)
  }

  test("insert past end throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v.insert(5, 99)
    }
  }

  test("insert at negative index throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v.insert(-1, 99)
    }
  }

  test("multiple inserts in sequence preserve total order") {
    // BUG #2: multiple inserts will compound the overwrite problem
    val v = NativeVector[Int](16)
    v.insert(0, 2)
    v.insert(0, 1)
    v.insert(0, 0)
    assert(v.length == 3)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 2)
  }

  test("insert interleaved with addOne preserves all elements") {
    // BUG #2
    val v = NativeVector[Int](16)
    v.addOne(10)
    v.addOne(30)
    v.insert(1, 20)
    assert(v.length == 3)
    assert(v(0) == 10)
    assert(v(1) == 20)
    assert(v(2) == 30)
  }

  // ── 4. prepend ──────────────────────────────────────────────────

  test("prepend inserts at front") {
    // BUG #2 (via insert)
    val v = filledIntVector(3)
    v.prepend(99)
    assert(v.length == 4)
    assert(v(0) == 99)
    assert(v(1) == 0)
    assert(v(2) == 1)
    assert(v(3) == 2)
  }

  test("multiple prepends reverse-prefix") {
    // BUG #2
    val v = NativeVector[Int](16)
    v.prepend(3)
    v.prepend(2)
    v.prepend(1)
    v.prepend(0)
    assert(v.length == 4)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 2)
    assert(v(3) == 3)
  }

  // ── 5. remove ──────────────────────────────────────────────────

  test("remove from beginning returns first element and shifts") {
    // Check shift right-to-left works
    val v = filledIntVector(5)
    val removed = v.remove(0)
    assert(removed == 0)
    assert(v.length == 4)
    assert(v(0) == 1)
    assert(v(1) == 2)
    assert(v(2) == 3)
    assert(v(3) == 4)
  }

  test("remove from middle returns correct element and shifts") {
    val v = filledIntVector(5)
    val removed = v.remove(2)
    assert(removed == 2)
    assert(v.length == 4)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 3)
    assert(v(3) == 4)
  }

  test("remove from end returns last element") {
    // BUG #3: srcOffset may be out-of-bounds for last element
    val v = filledIntVector(5)
    val removed = v.remove(4)
    assert(removed == 4)
    assert(v.length == 4)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 2)
    assert(v(3) == 3)
  }

  test("remove single element from size-1 vector") {
    // BUG #3: srcOffset = 1*byteSize = byteSize, segment boundary
    val v = filledIntVector(1)
    val removed = v.remove(0)
    assert(removed == 0)
    assert(v.length == 0)
  }

  test("remove from size-2 first element") {
    val v = filledIntVector(2)
    val removed = v.remove(0)
    assert(removed == 0)
    assert(v.length == 1)
    assert(v(0) == 1)
  }

  test("remove all elements one by one") {
    val v = filledIntVector(5)
    for (i <- 0 until 5) {
      val removed = v.remove(0)
      assert(removed == i, s"expected $i at step $i but got $removed")
    }
    assert(v.length == 0)
  }

  test("remove on empty vector throws IndexOutOfBoundsException") {
    val v = NativeVector[Int](16)
    intercept[IndexOutOfBoundsException] {
      v.remove(0)
    }
  }

  test("remove at negative index throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v.remove(-1)
    }
  }

  test("remove past end throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v.remove(5)
    }
  }

  // ── 6. apply ────────────────────────────────────────────────────

  test("apply returns correct values after sequential add") {
    val v = filledIntVector(10)
    for (i <- 0 until 10) {
      assert(v(i) == i, s"mismatch at index $i")
    }
  }

  test("apply at index == length throws IndexOutOfBoundsException") {
    // BUG #1: current impl allows reading at currentSize
    // because checkWithinBounds(i, i) uses hi > currentSize,
    // but i == currentSize passes (currentSize > currentSize is false)
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v(3) // currentSize == 3, valid indices are 0..2
    }
  }

  test("apply at index == length on empty vector throws") {
    // BUG #1
    val v = NativeVector[Int](16)
    intercept[IndexOutOfBoundsException] {
      v(0) // length == 0, no valid indices
    }
  }

  test("apply at negative index throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v(-1)
    }
  }

  test("apply at index past length throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v(10)
    }
  }

  test("apply after clear throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    v.clear()
    intercept[IndexOutOfBoundsException] {
      v(0)
    }
  }

  // ── 7. update ───────────────────────────────────────────────────

  test("update at valid index replaces element") {
    val v = filledIntVector(5)
    v.update(2, 99)
    assert(v(2) == 99)
    assert(v.length == 5)
  }

  test("update at index == length throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v.update(3, 99)
    }
  }

  test("update at negative index throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v.update(-1, 99)
    }
  }

  test("update at index past length throws IndexOutOfBoundsException") {
    val v = filledIntVector(3)
    intercept[IndexOutOfBoundsException] {
      v.update(10, 99)
    }
  }

  // ── 8. clear ────────────────────────────────────────────────────

  test("clear resets length to 0") {
    val v = filledIntVector(10)
    v.clear()
    assert(v.length == 0)
  }

  test("clear on empty vector leaves length at 0") {
    val v = NativeVector[Int](16)
    v.clear()
    assert(v.length == 0)
  }

  test("clear then addOne works") {
    val v = filledIntVector(5)
    v.clear()
    v.addOne(99)
    assert(v.length == 1)
    assert(v(0) == 99)
  }

  test("clear then insert works") {
    // BUG #2
    val v = filledIntVector(5)
    v.clear()
    v.insert(0, 10)
    v.insert(1, 20)
    assert(v.length == 2)
    assert(v(0) == 10)
    assert(v(1) == 20)
  }

  // ── 9. Iterator ─────────────────────────────────────────────────

  test("iterator over non-empty yields all elements in order") {
    val v = filledIntVector(5)
    val elems = v.iterator.toList
    assert(elems == List(0, 1, 2, 3, 4))
  }

  test("iterator over empty vector yields nothing") {
    val v = NativeVector[Int](16)
    val elems = v.iterator.toList
    assert(elems.isEmpty)
  }

  test("iterator next after exhaustion throws NoSuchElementException") {
    // BUG #5: Iterator.next() calls apply(currentSize) which
    // passes bounds check (BUG #1) and reads garbage.
    // Should throw NoSuchElementException when done.
    val v = filledIntVector(3)
    val it = v.iterator
    it.next() // 0
    it.next() // 1
    it.next() // 2
    intercept[NoSuchElementException] {
      it.next()
    }
  }

  test("iterator hasNext after exhaustion returns false") {
    val v = filledIntVector(3)
    val it = v.iterator
    it.next(); it.next(); it.next()
    assert(!it.hasNext)
  }

  test("iterator over large vector") {
    val v = NativeVector[Int](16)
    (0 until 1000).foreach(v.addOne)
    val elems = v.iterator.toList
    assert(elems.length == 1000)
    assert(elems.head == 0)
    assert(elems.last == 999)
  }

  test("multiple iterators on same vector are independent") {
    val v = filledIntVector(5)
    val it1 = v.iterator
    val it2 = v.iterator
    it1.next() // 0
    it1.next() // 1
    assert(it2.next() == 0) // it2 should start fresh
  }

  // ── 10. Bounds Checking ─────────────────────────────────────────

  test("negative index always throws regardless of length") {
    for (len <- Seq(0, 1, 5, 100)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException] {
        v(-1)
      }
    }
  }

  test("index equal to length always throws for read") {
    // BUG #1
    for (len <- Seq(0, 1, 5, 10)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException](v(len), s"read at length=$len should throw")
    }
  }

  test("index equal to length always throws for update") {
    for (len <- Seq(0, 1, 5, 10)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException](v.update(len, 99), s"update at length=$len should throw")
    }
  }

  test("insert at idx == length is valid for all lengths") {
    for (len <- Seq(0, 1, 5)) {
      val v = filledIntVector(len)
      v.insert(len, 99)
      assert(v.length == len + 1, s"failed at length=$len")
      assert(v(len) == 99, s"last element wrong at length=$len")
    }
  }

  test("insert at idx > length always throws") {
    for (len <- Seq(0, 1, 5)) {
      val v = filledIntVector(len)
      intercept[IndexOutOfBoundsException] {
        v.insert(len + 1, 99)
      }
    }
  }

  // ── 11. Capacity and Resize ────────────────────────────────────

  test("grow beyond initial capacity preserves all elements") {
    val v = NativeVector[Int](4)
    val n = 50
    (0 until n).foreach(v.addOne)
    assert(v.length == n)
    (0 until n).foreach(i => assert(v(i) == i))
  }

  test("grow after removing many elements") {
    val v = NativeVector[Int](16)
    (0 until 100).foreach(v.addOne)
    // Remove first 80
    (0 until 80).foreach(_ => v.remove(0))
    assert(v.length == 20)
    assert(v(0) == 80)
    assert(v(19) == 99)
    // Add more past current capacity
    (0 until 50).foreach(v.addOne)
    assert(v.length == 70)
    assert(v(0) == 80)
    assert(v(69) == 149)
  }

  test("addOne exactly fills initial capacity") {
    val initSize = 8
    val v = NativeVector[Int](initSize)
    (0 until initSize).foreach(v.addOne)
    assert(v.length == initSize)
    (0 until initSize).foreach(i => assert(v(i) == i))
  }

  test("addOne one past initial capacity triggers resize") {
    val initSize = 4
    val v = NativeVector[Int](initSize)
    (0 until initSize).foreach(v.addOne)
    v.addOne(99) // should trigger resize
    assert(v.length == initSize + 1)
    assert(v(initSize) == 99)
    // Verify first elements intact
    (0 until initSize).foreach(i => assert(v(i) == i))
  }

  test("repeated resize cycles preserve data") {
    val v = NativeVector[Int](2)
    (0 until 256).foreach(v.addOne) // multiple resize cycles
    assert(v.length == 256)
    (0 until 256).foreach(i => assert(v(i) == i))
  }

  // ── 12. Edge Cases: Double type ─────────────────────────────────

  test("Double vector addOne and apply") {
    val v = NativeVector[Double](16)
    v.addOne(3.14)
    v.addOne(2.71)
    assert(v(0) == 3.14)
    assert(v(1) == 2.71)
    assert(v.length == 2)
  }

  test("Double insert at beginning shifts existing elements") {
    // BUG #2
    val v = filledDoubleVector(3)
    v.insert(0, 99.9)
    assert(v.length == 4)
    assert(v(0) == 99.9)
    assert(v(1) == 0.0)
    assert(v(2) == 1.0)
    assert(v(3) == 2.0)
  }

  test("Double vector grows correctly") {
    val v = NativeVector[Double](2)
    (0 until 10).map(_.toDouble).foreach(v.addOne)
    assert(v.length == 10)
    (0 until 10).foreach(i => assert(v(i) == i.toDouble))
  }

  test("Double remove single element from size-1") {
    // BUG #3: Double is 8 bytes, makes boundary condition more likely
    val v = filledDoubleVector(1)
    val removed = v.remove(0)
    assert(removed == 0.0)
    assert(v.length == 0)
  }

  test("Double apply at index == length throws") {
    // BUG #1
    val v = filledDoubleVector(3)
    intercept[IndexOutOfBoundsException] {
      v(3)
    }
  }

  // ── 13. Edge Cases: Zero Initial Size ──────────────────────────

  test("zero initial size addOne works") {
    val v = NativeVector[Int](0)
    v.addOne(42)
    assert(v(0) == 42)
    assert(v.length == 1)
  }

  test("zero initial size multiple addOne works") {
    val v = NativeVector[Int](0)
    (0 until 10).foreach(v.addOne)
    assert(v.length == 10)
    (0 until 10).foreach(i => assert(v(i) == i))
  }

  test("zero initial size Double vector addOne works") {
    // BUG #4: Double is 8 bytes, alignedSize(0) = 8, needed = 8
    // ensureSize(1): 8 - 0 = 8 <= 8 → true → reallocates(currentSize*2=0)
    // This triggers reallocation every single addOne, may cause issues
    val v = NativeVector[Double](0)
    (0 until 5).map(_.toDouble).foreach(v.addOne)
    assert(v.length == 5)
    (0 until 5).foreach(i => assert(v(i) == i.toDouble))
  }

  // ── 14. Integration ─────────────────────────────────────────────

  test("mixed add, remove, insert sequence") {
    // BUG #2
    val v = NativeVector[Int](8)

    v.addOne(1)
    v.addOne(2)
    v.addOne(3)           // [1, 2, 3]
    v.insert(0, 0)        // expected: [0, 1, 2, 3]
    v.remove(4)           // no-op since length is now... depends on insert
    v.addOne(4)           // depends

    // If insert works: [0, 1, 2, 3, 4]
    // If insert overwrites: [0, 2, 3, garbage, 4]
    assert(v.length == 5)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 2)
    assert(v(3) == 3)
    assert(v(4) == 4)
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
    // BUG #2
    val v = filledIntVector(5)
    v.remove(2)           // [0, 1, 3, 4]
    v.insert(2, 99)       // expected: [0, 1, 99, 3, 4]
    assert(v.length == 5)
    assert(v(0) == 0)
    assert(v(1) == 1)
    assert(v(2) == 99)
    assert(v(3) == 3)
    assert(v(4) == 4)
  }

  test("clear then fill with more elements than original") {
    val v = filledIntVector(3)
    v.clear()
    (0 until 10).foreach(v.addOne)
    assert(v.length == 10)
    (0 until 10).foreach(i => assert(v(i) == i))
  }

  test("large number of elements with sequential access") {
    val n = 10000
    val v = NativeVector[Int](16)
    (0 until n).foreach(v.addOne)

    // Forward read
    for (i <- 0 until n) {
      assert(v(i) == i, s"forward mismatch at $i")
    }

    // Backward read
    for (i <- (n - 1) to 0 by -1) {
      assert(v(i) == i, s"backward mismatch at $i")
    }
  }

  test("vector never returns stale data after remove") {
    val v = NativeVector[Int](16)
    v.addOne(10)
    v.addOne(20)
    v.addOne(30)
    v.remove(1)
    assert(v(1) == 30, "element at idx 1 should be shifted value, not stale 20")
    assert(v.length == 2)
  }

  // ── 15. Contract: toString / equality (minimal) ─────────────────

  test("empty vector toString") {
    val v = NativeVector[Int](16)
    val s = v.toString
    assert(s.nonEmpty, "toString should not be empty")
  }

  test("non-empty vector toString") {
    val v = filledIntVector(3)
    val s = v.toString
    assert(s.nonEmpty)
    assert(s.contains("NativeVector"), "should contain class name")
  }

  test("NativeVector extends AbstractBuffer") {
    val v = NativeVector[Int](16)
    assert(v.isInstanceOf[scala.collection.mutable.AbstractBuffer[Int]])
  }
}