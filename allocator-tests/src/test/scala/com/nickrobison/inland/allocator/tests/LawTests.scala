package com.nickrobison.inland.allocator.tests

import com.nickrobison.inland.allocator.HeapAllocator
import com.nickrobison.inland.allocator.instances.IntLayout
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.{Discipline, FunSuiteDiscipline}
import com.nickrobison.inland.allocator.laws.AllocatorLaws

class LawTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Allocator", AllocatorLaws[Int](new HeapAllocator).laws)

}
