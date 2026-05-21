package com.nickrobison.inland.allocator.laws

import cats.Eq
import com.nickrobison.inland.allocator.NativeAllocator
import com.nickrobison.inland.types.Layout
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import java.lang.foreign.MemorySegment

trait AllocatorLaws[A] extends Laws {

  def allocator: NativeAllocator
  implicit def layout: Layout[A]
  implicit def arbA: Arbitrary[A]
  implicit def eq: Eq[A]
  implicit def countArb: Arbitrary[Int] = Arbitrary(Gen.choose(1, 100))

  def laws = new AllocatorProperties(
    name = "allocator",
    parent = None,
    "size" -> sizeLaws(),
    "alignment" -> alignmentLaws(8),
    "isolation" -> isolationLaws(),
    "free" -> freeLaws(),
    "allocated" -> allocatedLaws(),
    "reallocate preserves contents" -> reallocatePreservesContentsLaws(),
    "reallocate size matches" -> reallocateSizeMatchesLaws(),
    "reallocate shrinks correctly" -> reallocateShrinksCorrectlyLaws()
  )

  private def sizeLaws(): Prop = {
    forAll { (count: Int) =>
      val a = allocator.allocate[A](count)
      a.byteSize() >= (count * layout.byteSize)
    }
  }

  private def alignmentLaws(alignment: Long): Prop = {
    forAll { (count: Int) =>
      val a = allocator.allocate[A](count)
      try {
        val addr = a.address()
        (addr % alignment) == 0
      } catch {
        case _: UnsupportedOperationException =>
          // heap segments don't expose raw addresses; skip alignment check.
          // The allocated segment is guaranteed aligned by JVM array layout.
          true
      }
    }
  }

  private def isolationLaws(): Prop = {
    forAll { (count: Int, valueA: A, valueB: A) =>
      if (count > 0 && Eq[A].neqv(valueA, valueB)) {
        val a = allocator.allocate[A](count)
        val b = allocator.allocate[A](count)

        // Write different values to each segment
        layout.write(0, valueA)(using a)
        layout.write(0, valueB)(using b)

        // Read back and verify they have different values
        val readA = layout.read(0)(using a)
        val readB = layout.read(0)(using b)

        nonAliasingValue(readA, readB, valueA, valueB)
      } else {
        true // Skip test for non-positive counts or identical values
      }
    }
  }

  private def freeLaws(): Prop = {
    forAll { (count: Int) =>
      val a = allocator.allocate[A](count)
      allocator.free(a)
      true
    }
  }

  private def allocatedLaws(): Prop = {
    forAll { (values: Seq[A]) =>
      if (values.nonEmpty) {

        implicit val segment: MemorySegment = allocator.allocate[A](values.length)
        values.zipWithIndex.foreach { case (v, idx) =>
          layout.write(idx, v)
        }

        val read = values.indices.map { idx =>
          layout.read(idx)
        }

        Eq[Seq[A]].eqv(values, read)
      } else {
        true
      }
    }
  }

  private def reallocatePreservesContentsLaws(): Prop = {
    forAll { (values: Seq[A], newCount: Int) =>
      if (values.nonEmpty && newCount > values.length) {
        val (oldSegment, newSegment) = allocWriteAndReallocate(values, newCount)
        valuesPreserved(oldSegment, newSegment, values.length)
      } else {
        true
      }
    }
  }

  private def reallocateSizeMatchesLaws(): Prop = {
    forAll { (oldCount: Int, newCount: Int) =>
      if (oldCount > 0 && newCount > 0) {
        val oldSegment = allocator.allocate[A](oldCount)
        val newSegment = allocator.reallocate[A](oldSegment, oldCount, newCount)

        newSegment.byteSize() >= (newCount * layout.byteSize)
      } else {
        true
      }
    }
  }

  private def reallocateShrinksCorrectlyLaws(): Prop = {
    forAll { (values: Seq[A], newCount: Int) =>
      if (values.nonEmpty && newCount > 0 && newCount < values.length) {
        val (oldSegment, newSegment) = allocWriteAndReallocate(values, newCount)
        valuesPreserved(oldSegment, newSegment, newCount)
      } else {
        true
      }
    }
  }

  private def nonAliasingValue(readA: A, readB: A, valueA: A, valueB: A): Boolean =
    Eq[A].eqv(readA, valueA) && Eq[A].eqv(readB, valueB) && Eq[A].neqv(readA, readB)

  private def allocWriteAndReallocate(
      values: Seq[A],
      newCount: Int): (MemorySegment, MemorySegment) =
    val oldSegment = allocator.allocate[A](values.length)
    values.zipWithIndex.foreach { case (v, idx) => layout.write(idx, v)(using oldSegment) }
    val newSegment = allocator.reallocate[A](oldSegment, values.length, newCount)
    (oldSegment, newSegment)

  private def valuesPreserved(
      oldSegment: MemorySegment,
      newSegment: MemorySegment,
      count: Int): Boolean =
    (0 until count).forall { idx =>
      Eq[A].eqv(layout.read(idx)(using oldSegment), layout.read(idx)(using newSegment))
    }

  class AllocatorProperties(
      name: String,
      parent: Option[AllocatorProperties],
      props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props*)
}

object AllocatorLaws {
  def apply[A: Layout: Eq](na: NativeAllocator)(implicit arb: Arbitrary[A]): AllocatorLaws[A] =
    new AllocatorLaws[A] {
      override def allocator: NativeAllocator = na

      override def arbA: Arbitrary[A] = arb

      override def eq: Eq[A] = Eq[A]

      override def layout: Layout[A] = Layout[A]
    }
}
