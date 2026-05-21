package com.nickrobison.inland.allocator.tests

import cats.Eq
import com.nickrobison.inland.types.Layout
import org.scalacheck.{Arbitrary, Gen}

import java.lang.foreign.{MemoryLayout, MemorySegment, ValueLayout}

final case class SimpleClass(age: Int, success: Double, favoriteNumber: Long)

object SimpleClass {
  given Layout[SimpleClass] = new Layout[SimpleClass] {

    override def memoryLayout: MemoryLayout = MemoryLayout.structLayout(
      ValueLayout.JAVA_INT.withName("age"),
      MemoryLayout.paddingLayout(4),
      ValueLayout.JAVA_DOUBLE.withName("success"),
      ValueLayout.JAVA_LONG.withName("favoriteNumber")
    )

    private final val ageHandle = memoryLayout.varHandle(MemoryLayout.PathElement.groupElement("age"))
    private final val successHandle = memoryLayout.varHandle(MemoryLayout.PathElement.groupElement("success"))
    private final val numHandle = memoryLayout.varHandle(MemoryLayout.PathElement.groupElement("favoriteNumber"))

    override def write(offset: Long, value: SimpleClass)(using segment: MemorySegment): Unit = {
      val byteOffset = offset * byteSize
      ageHandle.set(segment, byteOffset, value.age)
      successHandle.set(segment, byteOffset, value.success)
      numHandle.set(segment, byteOffset, value.favoriteNumber)
    }

    override def read(offset: Long)(using segment: MemorySegment): SimpleClass = {
      val byteOffset = offset * byteSize
      val age = ageHandle.get(segment, byteOffset).asInstanceOf[Int]
      val success = successHandle.get(segment, byteOffset).asInstanceOf[Double]
      val num = numHandle.get(segment, byteOffset).asInstanceOf[Long]
      SimpleClass(age, success, num)
    }
  }

  lazy val gen: Gen[SimpleClass] = for {
    age <- Gen.size
    success <- Gen.choose(0.0, 100.0)
    num <- Gen.size
  } yield SimpleClass(age, success, num)

  given Arbitrary[SimpleClass] = Arbitrary(gen)

  given Eq[SimpleClass] = Eq.fromUniversalEquals
}
