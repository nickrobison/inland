package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.{ArenaAllocator, HeapAllocator, NativeAllocator}
import com.nickrobison.inland.allocator.instances.given
import com.nickrobison.inland.types.Layout
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Arbitrary
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.lang.foreign.Arena

class NativeVectorLawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  given Arena = Arena.ofShared()
  given NativeAllocator = ArenaAllocator(128000L)

  checkAll("NativeVector[Int]", NativeVectorLaws[Int].nativeVectorLaws)
  checkAll(
    "NativeVector[Double]",
    NativeVectorLaws[Double](using
      summon[Layout[Double]],
      HeapAllocator(),
      summon[Arbitrary[Double]]).nativeVectorLaws)
  checkAll(
    "NativeVector[Long]",
    NativeVectorLaws[Long](using
      summon[Layout[Long]],
      HeapAllocator(),
      summon[Arbitrary[Long]]).nativeVectorLaws)
  checkAll(
    "NativeVector[Float]",
    NativeVectorLaws[Float](using
      summon[Layout[Float]],
      HeapAllocator(),
      summon[Arbitrary[Float]]).nativeVectorLaws)
  checkAll(
    "NativeVector[Char]",
    NativeVectorLaws[Char](using
      summon[Layout[Char]],
      HeapAllocator(),
      summon[Arbitrary[Char]]).nativeVectorLaws)

}
