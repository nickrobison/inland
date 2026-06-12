package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.instances.array.{DoubleInstances, IntInstances}
import com.nickrobison.inland.executor.simd.ArithOpsLaws.given
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class ArithAlgebraTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Int[Preferred]AlgebraTests", ArithOpsLaws[Int].laws(using IntInstances.intPreferred))
  checkAll("Int[Max]AlgebraTests", ArithOpsLaws[Int].laws(using IntInstances.intMax))
  checkAll("Int[64]AlgebraTests", ArithOpsLaws[Int].laws(using IntInstances.int64))
  checkAll("Double[256]AlgebraTests", ArithOpsLaws[Double].laws(using DoubleInstances.double256))
  checkAll("Double[512]AlgebraTests", ArithOpsLaws[Double].laws(using DoubleInstances.double512))
}
