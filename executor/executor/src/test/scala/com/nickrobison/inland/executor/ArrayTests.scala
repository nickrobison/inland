package com.nickrobison.inland.executor

import cats.Eq
import com.nickrobison.inland.executor.instances.array.IntInstances.{int64, intPreferred}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import com.nickrobison.inland.executor.instances.array.given

import java.util

class ArrayTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  given eq: Eq[SimdVector[Int]] with {
    def eqv(x: SimdVector[Int], y: SimdVector[Int]): Boolean = {
      util.Arrays.equals(x.underlying.toIntArray, y.underlying.toIntArray)
    }
  }

  given BitwiseOps[Int] = intPreferred

  given Arbitrary[Array[Int]] = Arbitrary(
    for {
      n <- Gen.choose(0, 1000)
      elements <- Gen.listOfN(n, Gen.chooseNum(-128, 128))
    } yield elements.toArray
  )

  given Arbitrary[VectorIdx] = Arbitrary(Gen.chooseNum(0, 100).map(VectorIdx.apply))

  checkAll("VectorBatchLaws", VectorBatchLaws[Array, Int].laws)
  checkAll("ArithOpsLaws", ArithOpsLaws[Array, Int].laws)
}
