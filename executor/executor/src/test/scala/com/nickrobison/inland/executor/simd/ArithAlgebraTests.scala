package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.instances.array.{DoubleInstances, FloatInstances, IntInstances, LongInstances, ShortInstances}
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
  checkAll("Float[256]AlgebraTests", ArithOpsLaws[Float].laws(using FloatInstances.float256))
  checkAll("Float[512]AlgebraTests", ArithOpsLaws[Float].laws(using FloatInstances.float512))
  checkAll("Long[256]AlgebraTests", ArithOpsLaws[Long].laws(using LongInstances.long256))
  checkAll("Long[Preferred]AlgebraTests", ArithOpsLaws[Long].laws(using LongInstances.longPref))
  checkAll("Long[Max]AlgebraTests", ArithOpsLaws[Long].laws(using LongInstances.longMax))
  checkAll("Short[128]AlgebraTests", ArithOpsLaws[Short].laws(using ShortInstances.short128))
  checkAll("Short[Preferred]AlgebraTests", ArithOpsLaws[Short].laws(using ShortInstances.shortPref))
  checkAll("Short[Max]AlgebraTests", ArithOpsLaws[Short].laws(using ShortInstances.shortMax))
}
