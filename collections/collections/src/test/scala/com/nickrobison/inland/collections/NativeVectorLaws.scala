package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.NativeAllocator
import com.nickrobison.inland.types.Layout
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import java.util.NoSuchElementException

class NativeVectorLaws[A](using layout: Layout[A], allocator: NativeAllocator, arbA: Arbitrary[A])
    extends Laws {

  def nativeVectorLaws: RuleSet = new DefaultRuleSet(
    name = "NativeVector",
    parent = None,
    "initial length zero" -> initialLengthZero,
    "addOne then apply" -> addOneThenApply(),
    "addOne increases length" -> addOneIncreasesLength(),
    "update then apply" -> updateThenApply(),
    "sequential add read back" -> sequentialAddReadBack,
    "clear resets length" -> clearResetsLength,
    "insert then apply" -> insertThenApply(),
    "remove decreases length" -> removeDecreasesLength(),
    "remove returns correct element" -> removeReturnsCorrectElement(),
    "insert shifts elements right" -> insertShiftsRight(),
    "insert preserves all elements" -> insertPreservesAllElements(),
    "prepend inserts at front" -> prependInsertsAtFront(),
    "insertAll preserves all elements" -> insertAllPreservesAllElements(),
    "remove batch preserves elements" -> removeBatchPreservesElements(),
    "patchInPlace correctness" -> patchInPlaceCorrectness(),
    "iterator exhaustion throws NoSuchElementException" -> iteratorExhaustionThrows()
  )

  // ── laws ──────────────────────────────────────────────────────────

  private def initialLengthZero: Prop = {
    val vec = NativeVector[A](16)
    Prop(vec.length == 0)
  }

  private def addOneThenApply(): Prop = {
    forAll { (x: A) =>
      val vec = NativeVector[A](16)
      vec.addOne(x)
      vec(vec.length - 1) == x
    }
  }

  private def addOneIncreasesLength(): Prop = {
    forAll { (x: A) =>
      val vec = NativeVector[A](16)
      val before = vec.length
      vec.addOne(x)
      vec.length == before + 1
    }
  }

  private def updateThenApply(): Prop = {
    forAll { (x: A, y: A) =>
      val vec = NativeVector[A](16)
      vec.addOne(x)
      val idx = vec.length - 1
      vec.update(idx, y)
      vec(idx) == y
    }
  }

  private def sequentialAddReadBack: Prop = {
    forAll { (values: Seq[A]) =>
      if (values.nonEmpty && values.length <= 64) {
        val vec = NativeVector[A](values.length)
        values.foreach(vec.addOne)
        val readBack = (0 until vec.length).map(vec.apply)
        values == readBack
      } else {
        true
      }
    }
  }

  private def clearResetsLength: Prop = {
    forAll { (values: Seq[A]) =>
      if (values.nonEmpty && values.length <= 64) {
        val vec = NativeVector[A](values.length)
        values.foreach(vec.addOne)
        vec.clear()
        vec.length == 0
      } else {
        true
      }
    }
  }

  private def insertThenApply(): Prop = {
    forAll { (x: A, y: A) =>
      val vec = NativeVector[A](16)
      vec.addOne(x)
      vec.insert(0, y)
      vec.head == y
    }
  }

  private def removeDecreasesLength(): Prop = {
    forAll { (values: Seq[A]) =>
      if (values.length >= 2 && values.length <= 64) {
        val vec = NativeVector[A](values.length)
        values.foreach(vec.addOne)
        val before = vec.length
        vec.remove(0)
        vec.length == before - 1
      } else {
        true
      }
    }
  }

  private def removeReturnsCorrectElement(): Prop = {
    forAll { (values: Seq[A]) =>
      if (values.nonEmpty && values.length <= 64) {
        val vec = NativeVector[A](values.length)
        values.foreach(vec.addOne)
        val removed = vec.remove(0)
        removed == values.head && vec.length == values.length - 1
      } else {
        true
      }
    }
  }

  private def insertShiftsRight(): Prop = {
    forAll { (before: Seq[A], elem: A) =>
      if (before.nonEmpty && before.length <= 64) {
        val vec = NativeVector[A](before.length)
        before.foreach(vec.addOne)
        val idx = before.length / 2
        vec.insert(idx, elem)
        vec(idx) == elem && vec(idx + 1) == before(idx)
      } else {
        true
      }
    }
  }

  private def insertPreservesAllElements(): Prop = {
    forAll { (before: Seq[A], elem: A) =>
      if (before.nonEmpty && before.length <= 63) {
        val vec = NativeVector[A](before.length)
        before.foreach(vec.addOne)
        val idx = before.length / 2
        vec.insert(idx, elem)
        val expected = before.take(idx) ++ Seq(elem) ++ before.drop(idx)
        val actual = (0 until vec.length).map(vec.apply)
        actual == expected
      } else {
        true
      }
    }
  }
  private def prependInsertsAtFront(): Prop = {
    forAll { (before: Seq[A], elem: A) =>
      if (before.nonEmpty && before.length <= 64) {
        val vec = NativeVector[A](before.length)
        before.foreach(vec.addOne)
        vec.prepend(elem)
        val expected = elem +: before
        val actual = (0 until vec.length).map(vec.apply)
        actual == expected
      } else {
        true
      }
    }
  }

  private def insertAllPreservesAllElements(): Prop = {
    forAll { (before: Seq[A], elems: Seq[A]) =>
      if (before.nonEmpty && elems.nonEmpty && before.length + elems.length <= 64) {
        val vec = NativeVector[A](before.length)
        before.foreach(vec.addOne)
        val idx = before.length / 2
        vec.insertAll(idx, elems)
        val expected = before.take(idx) ++ elems ++ before.drop(idx)
        val actual = (0 until vec.length).map(vec.apply)
        actual == expected
      } else {
        true
      }
    }
  }

  private def removeBatchPreservesElements(): Prop = {
    forAll { (before: Seq[A]) =>
      if (before.length >= 2 && before.length <= 64) {
        val removeIdx = before.length / 3
        val removeCount = math.min(2, before.length - removeIdx)
        val vec = NativeVector[A](before.length)
        before.foreach(vec.addOne)
        vec.remove(removeIdx, removeCount)
        val expected = before.take(removeIdx) ++ before.drop(removeIdx + removeCount)
        val actual = (0 until vec.length).map(vec.apply)
        actual == expected
      } else {
        true
      }
    }
  }

  private def patchInPlaceCorrectness(): Prop = {
    forAll { (before: Seq[A], patch: Seq[A]) =>
      if (before.nonEmpty && before.length <= 50) {
        val patchList = patch.take(10).toList
        val from = before.length / 2
        val replaced = math.min(2, before.length - from)
        val vec = NativeVector[A](before.length)
        before.foreach(vec.addOne)
        vec.patchInPlace(from, patchList, replaced)
        val expected = before.take(from) ++ patchList ++ before.drop(from + replaced)
        val actual = (0 until vec.length).map(vec.apply)
        actual == expected
      } else {
        true
      }
    }
  }

  private def iteratorExhaustionThrows(): Prop = {
    forAll { (values: Seq[A]) =>
      if (values.nonEmpty && values.length <= 16) {
        val vec = NativeVector[A](values.length)
        values.foreach(vec.addOne)
        val it = vec.iterator
        values.foreach(_ => it.next())
        try {
          it.next()
          false
        } catch {
          case _: NoSuchElementException => true
        }
      } else {
        true
      }
    }
  }
}

object NativeVectorLaws {
  def apply[A](using Layout[A], NativeAllocator, Arbitrary[A]): NativeVectorLaws[A] =
    new NativeVectorLaws[A]
}
