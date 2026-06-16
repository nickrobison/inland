package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.instances.array.{DoubleInstances, FloatInstances}
import com.nickrobison.inland.executor.simd.FloatOpsLaws.given
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class FloatAlgebraTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  given ArbDouble: Arbitrary[Double] = Arbitrary(
    Gen.chooseNum(-700.0, 700.0))

  given ArbFloat: Arbitrary[Float] = Arbitrary(
    Gen.chooseNum(-80.0f, 80.0f))

  checkAll("Double[256]FloatOps", FloatOpsLaws[Double].laws(using DoubleInstances.double256))
  checkAll("Double[512]FloatOps", FloatOpsLaws[Double].laws(using DoubleInstances.double512))

  checkAll("Float[256]FloatOps", FloatOpsLaws[Float].laws(using FloatInstances.float256))
  checkAll("Float[512]FloatOps", FloatOpsLaws[Float].laws(using FloatInstances.float512))
}
