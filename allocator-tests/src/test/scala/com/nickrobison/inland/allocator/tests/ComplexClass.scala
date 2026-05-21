package com.nickrobison.inland.allocator.tests

import cats.Eq
import cats.syntax.eq.*
import com.nickrobison.inland.types.Layout
import org.scalacheck.{Arbitrary, Gen}

import java.util.Arrays
import java.lang.foreign.{MemoryLayout, MemorySegment, ValueLayout}
import java.util

final case class ComplexClass(data: Array[Byte], simple: SimpleClass)

object ComplexClass {

  private val dataLayout = MemoryLayout.sequenceLayout(8, ValueLayout.JAVA_BYTE)

  given Layout[ComplexClass] = new Layout[ComplexClass] {

    override def memoryLayout: MemoryLayout = MemoryLayout.structLayout(
      dataLayout.withName("data"),
      summon[Layout[SimpleClass]].memoryLayout.withName("simple")
    )

    private final val dataHandle = memoryLayout.varHandle(
      MemoryLayout.PathElement.groupElement("data"),
      MemoryLayout.PathElement.sequenceElement()
    )

    private val simpleLayout = summon[Layout[SimpleClass]]
    private val dataByteSize = 8L

    override def write(offset: Long, value: ComplexClass)(using segment: MemorySegment): Unit = {
      val byteOffset = offset * byteSize

      var i = 0
      while (i < 8) {
        dataHandle.set(segment, byteOffset, i.toLong, value.data(i))
        i += 1
      }

      val simpleSegment = segment.asSlice(byteOffset + dataByteSize, simpleLayout.byteSize)
      simpleLayout.write(0, value.simple)(using simpleSegment)
    }

    override def read(offset: Long)(using segment: MemorySegment): ComplexClass = {
      val byteOffset = offset * byteSize

      val data = new Array[Byte](8)
      var i = 0
      while (i < 8) {
        data(i) = dataHandle.get(segment, byteOffset, i.toLong).asInstanceOf[Byte]
        i += 1
      }

      val simpleSegment = segment.asSlice(byteOffset + dataByteSize, simpleLayout.byteSize)
      val simple = simpleLayout.read(0)(using simpleSegment)

      ComplexClass(data, simple)
    }
  }

    lazy val complexGen: Gen[ComplexClass] = for {
      data <- Gen.containerOfN[Array, Byte](8, Gen.choose(Byte.MinValue, Byte.MaxValue))
      simple <- SimpleClass.gen
    } yield ComplexClass(data, simple)

    given Arbitrary[ComplexClass] = Arbitrary(complexGen)

    given Eq[ComplexClass] = Eq.instance { (x, y) =>
      util.Arrays.equals(x.data, y.data) && x.simple === y.simple
    }
  }
