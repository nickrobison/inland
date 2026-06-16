package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.instances.array.{DoubleInstances, FloatInstances, IntInstances, LongInstances, ShortInstances}
import com.nickrobison.inland.executor.simd.ArithOpsLaws.given
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class OrderAlgebraTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Int[Preferred]OrderTests", OrderOpsLaws[Int].laws(using IntInstances.intPreferred))
  checkAll("Int[Max]OrderTests", OrderOpsLaws[Int].laws(using IntInstances.intMax))
  checkAll("Int[64]OrderTests", OrderOpsLaws[Int].laws(using IntInstances.int64))
  checkAll("Long[256]OrderTests", OrderOpsLaws[Long].laws(using LongInstances.long256))
  checkAll("Long[Preferred]OrderTests", OrderOpsLaws[Long].laws(using LongInstances.longPref))
  checkAll("Long[Max]OrderTests", OrderOpsLaws[Long].laws(using LongInstances.longMax))
  checkAll("Short[128]OrderTests", OrderOpsLaws[Short].laws(using ShortInstances.short128))
  checkAll("Short[Preferred]OrderTests", OrderOpsLaws[Short].laws(using ShortInstances.shortPref))
  checkAll("Short[Max]OrderTests", OrderOpsLaws[Short].laws(using ShortInstances.shortMax))

  given ArbDouble: Arbitrary[Double] = Arbitrary(
    Gen.chooseNum(Double.MinValue, Double.MaxValue))

  checkAll("Double[256]OrderTests", OrderOpsLaws[Double].laws(using DoubleInstances.double256))
  checkAll("Double[512]OrderTests", OrderOpsLaws[Double].laws(using DoubleInstances.double512))

  checkAll("Float[256]OrderTests", OrderOpsLaws[Float].laws(using FloatInstances.float256))
  checkAll("Float[512]OrderTests", OrderOpsLaws[Float].laws(using FloatInstances.float512))
}
