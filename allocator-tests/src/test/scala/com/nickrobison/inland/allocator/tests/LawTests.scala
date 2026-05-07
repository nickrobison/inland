package com.nickrobison.inland.allocator.tests

import com.nickrobison.inland.allocator.HeapAllocator
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.{Discipline, FunSuiteDiscipline}
import com.nickrobison.inland.allocator.laws.{AllocatorLaws, LayoutLaws}
import cats.implicits.*
import com.nickrobison.inland.allocator.instances.given

class LawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Allocator", AllocatorLaws[Int](new HeapAllocator).laws)

  checkAll("Layout[Int]", LayoutLaws[Int].laws)
  checkAll("Layout[Double]", LayoutLaws[Double].laws)
  checkAll("Layout[Long]", LayoutLaws[Long].laws)
  checkAll("Layout[Float]", LayoutLaws[Float].laws)
  checkAll("Layout[Byte]", LayoutLaws[Byte].laws)
  checkAll("Layout[Char]", LayoutLaws[Char].laws)

}
