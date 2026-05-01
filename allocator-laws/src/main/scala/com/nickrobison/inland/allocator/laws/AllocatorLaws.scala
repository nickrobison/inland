package com.nickrobison.inland.allocator.laws

import cats.Eq
import com.nickrobison.inland.allocator.{Layout, NativeAllocator}
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
    // "isolation" -> isolationLaws(), // Later
    "free" -> freeLaws(),
    "allocated" -> allocatedLaws()
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
      val addr = a.address()
      (addr % alignment) == 0
    }
  }

  private def isolationLaws(): Prop = {
    forAll { (count: Int) =>
      val a = allocator.allocate[A](count)
      val b = allocator.allocate[A](count)
      (a.address() != b.address()) || (a eq b)
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

  class AllocatorProperties(
                           name: String,
                           parent: Option[AllocatorProperties],
                           props: (String, Prop)*
                           ) extends DefaultRuleSet(name, parent, props: _*)
}

object AllocatorLaws {
  def apply[A: Layout: Eq](na: NativeAllocator)(implicit arb: Arbitrary[A]): AllocatorLaws[A] = new AllocatorLaws[A] {
    override def allocator: NativeAllocator = na

    override def arbA: Arbitrary[A] = arb

    override def eq: Eq[A] = Eq[A]

    override def layout: Layout[A] = Layout[A]
  }
}