package com.nickrobison.inland.allocator.tests

import cats.implicits.*
import com.nickrobison.inland.allocator.instances.given
import com.nickrobison.inland.allocator.laws.{AllocatorLaws, LayoutLaws}
import com.nickrobison.inland.allocator.{ArenaAllocator, HeapAllocator}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.lang.foreign.Arena

class LawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  given Arena = Arena.ofShared()

  checkAll("HeapAllocator", AllocatorLaws[Int](new HeapAllocator).laws)
  checkAll("ArenaAllocator", AllocatorLaws[Double](ArenaAllocator(128L)).laws)


  checkAll("Layout[Int]", LayoutLaws[Int].laws)
  checkAll("Layout[Double]", LayoutLaws[Double].laws)
  checkAll("Layout[Long]", LayoutLaws[Long].laws)
  checkAll("Layout[Float]", LayoutLaws[Float].laws)
  checkAll("Layout[Byte]", LayoutLaws[Byte].laws)
  checkAll("Layout[Char]", LayoutLaws[Char].laws)

}
