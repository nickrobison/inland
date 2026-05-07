package com.nickrobison.inland.allocator.laws

import cats.Eq
import com.nickrobison.inland.allocator.{HeapAllocator, Layout, NativeAllocator}
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import java.lang.foreign.MemorySegment

trait LayoutLaws[A] extends Laws {

  implicit def layout: Layout[A]
  implicit def arbA: Arbitrary[A]
  implicit def eq: Eq[A]
  implicit def countArb: Arbitrary[Int] = Arbitrary(Gen.choose(1, 100))

  def laws = new LayoutProperties(
    name = "layout",
    parent = None,
    "write read consistency" -> writeReadConsistencyLaws(),
    "byte size consistency" -> byteSizeConsistencyLaws(),
    "sequential writes" -> sequentialWritesLaws(),
    "non-aliasing writes" -> nonAliasingWritesLaws()
  )

  private def writeReadConsistencyLaws(): Prop = {
    forAll { (value: A) =>
      val allocator = new HeapAllocator()
      val segment = allocator.allocate[A](1)

      layout.write(0, value)(using segment)
      val readValue = layout.read(0)(using segment)

      Eq[A].eqv(value, readValue)
    }
  }

  private def byteSizeConsistencyLaws(): Prop = {
    Prop(layout.byteSize == layout.memoryLayout.byteSize())
  }

  private def sequentialWritesLaws(): Prop = {
    forAll { (values: Seq[A]) =>
      if (values.nonEmpty && values.length <= 10) { // Limit to prevent excessive memory usage
        val allocator = new HeapAllocator()
        val segment = allocator.allocate[A](values.length)

        // Write all values sequentially
        values.zipWithIndex.foreach { case (v, idx) =>
          layout.write(idx, v)(segment)
        }

        // Read them back and verify
        val readValues = values.indices.map { idx =>
          layout.read(idx)(segment)
        }

        Eq[Seq[A]].eqv(values, readValues)
      } else {
        true
      }
    }
  }

  private def nonAliasingWritesLaws(): Prop = {
    forAll { (valueA: A, valueB: A) =>
      if (Eq[A].neqv(valueA, valueB)) {
        val allocator = new HeapAllocator()
        val segment = allocator.allocate[A](2)

        // Write different values at offset 0 and offset 1
        layout.write(0, valueA)(segment)
        layout.write(1, valueB)(segment)

        // Read back and verify they have different values
        val readA = layout.read(0)(segment)
        val readB = layout.read(1)(segment)

        Eq[A].eqv(readA, valueA) && Eq[A].eqv(readB, valueB) && Eq[A].neqv(readA, readB)
      } else {
        true // Skip test for identical values
      }
    }
  }

  class LayoutProperties(
                         name: String,
                         parent: Option[LayoutProperties],
                         props: (String, Prop)*
                         ) extends DefaultRuleSet(name, parent, props*)
}

object LayoutLaws {
  def apply[A: Layout](implicit l: Layout[A], arb: Arbitrary[A], eqz: Eq[A]): LayoutLaws[A] = new LayoutLaws[A] {
    override implicit def layout: Layout[A] = l
    override implicit def arbA: Arbitrary[A] = arb
    override implicit def eq: Eq[A] = eqz
  }
}


