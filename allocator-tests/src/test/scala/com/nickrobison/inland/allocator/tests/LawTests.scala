package com.nickrobison.inland.allocator.tests

import com.nickrobison.inland.allocator.HeapAllocator
import com.nickrobison.inland.allocator.instances.{IntLayout, DoubleLayout}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.{Discipline, FunSuiteDiscipline}
import com.nickrobison.inland.allocator.laws.{AllocatorLaws, LayoutLaws}
import cats.implicits._

class LawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

//  checkAll("Allocator", AllocatorLaws[Int](new HeapAllocator).laws)

  checkAll("Layout[Int]", LayoutLaws[Int].laws)
  checkAll("Layout[Double]", LayoutLaws[Double].laws)

}
