package com.nickrobison.inland.executor.simd

import cats.Eq
import com.nickrobison.inland.executor.instances.array.{DoubleInstances, IntInstances}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class ArithAlgebraTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  given ArithOps[Int] = IntInstances.intPreferred
  given ArithOps[Double] = DoubleInstances.double256

  given eqInt: Eq[Array[Int]] with {
    def eqv(x: Array[Int], y: Array[Int]): Boolean = x.sameElements(y)
  }

  given eqDouble: Eq[Array[Double]] with {
    def eqv(x: Array[Double], y: Array[Double]): Boolean = x.sameElements(y)
  }

  given arbI: Arbitrary[Array[Int]] = Arbitrary(
    for {
      elements <- Gen.listOfN(IntInstances.intPreferred.lanes, Gen.chooseNum(-128, 128))
    } yield elements.toArray
  )

  given arbD: Arbitrary[Array[Double]] = Arbitrary(
    for {
      elements <- Gen.listOfN(DoubleInstances.double256.lanes, Gen.chooseNum(-128.0, 128.0))
    } yield elements.toArray
  )

  checkAll("IntAlgebraTests", ArithOpsLaws[Int].laws)
  checkAll("DoubleAlgebraTests", ArithOpsLaws[Double].laws)
}
