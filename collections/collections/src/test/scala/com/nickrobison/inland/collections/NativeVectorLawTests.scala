package com.nickrobison.inland.collections

import com.nickrobison.inland.allocator.{ArenaAllocator, HeapAllocator, NativeAllocator}
import com.nickrobison.inland.allocator.instances.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.lang.foreign.Arena

class NativeVectorLawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  
  given Arena = Arena.ofShared()
  given NativeAllocator = ArenaAllocator(128000L)

  checkAll("NativeVector[Int]", NativeVectorLaws[Int].nativeVectorLaws)
  checkAll("NativeVector[Double]", NativeVectorLaws[Double](using summon, HeapAllocator(), summon).nativeVectorLaws)
  checkAll("NativeVector[Long]", NativeVectorLaws[Long](using summon, HeapAllocator(), summon).nativeVectorLaws)

}

