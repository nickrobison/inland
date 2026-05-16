package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.{Layout, NativeAllocator}
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

class NativeVectorLaws[A](using layout: Layout[A], allocator: NativeAllocator, arbA: Arbitrary[A]) extends Laws {

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
}

object NativeVectorLaws {
  def apply[A](using Layout[A], NativeAllocator, Arbitrary[A]): NativeVectorLaws[A] = new NativeVectorLaws[A]
}
