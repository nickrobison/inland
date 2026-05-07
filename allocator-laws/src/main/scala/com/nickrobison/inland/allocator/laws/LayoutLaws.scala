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

  def laws = new LayoutProperties(
    name = "layout",
    parent = None,
    "write read consistency" -> writeReadConsistencyLaws(),
    "byte size consistency" -> byteSizeConsistencyLaws(),
    "sequential writes" -> sequentialWritesLaws()
  )

  private def writeReadConsistencyLaws(): Prop = {
    forAll { (value: A) =>
      val allocator = new HeapAllocator()
      val segment = allocator.allocate[A](1)

      layout.write(0, value)(segment)
      val readValue = layout.read(0)(segment)

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

  class LayoutProperties(
                         name: String,
                         parent: Option[LayoutProperties],
                         props: (String, Prop)*
                         ) extends DefaultRuleSet(name, parent, props*)
}

object LayoutLaws {
  def apply[A: {Layout, Eq}](implicit arb: Arbitrary[A]): LayoutLaws[A] = new LayoutLaws[A] {
    override implicit def layout: Layout[A] = Layout[A]
    override implicit def arbA: Arbitrary[A] = arb
    override implicit def eq: Eq[A] = Eq[A]
  }
}