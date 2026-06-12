package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.instances.array.IntInstances
import com.nickrobison.inland.executor.simd.ArithOpsLaws.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class BitwiseAlgebraTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Int[Preferred]BitwiseTests", BitwiseOpsLaws[Int].laws(using IntInstances.intPreferred))
  checkAll("Int[Max]BitwiseTests", BitwiseOpsLaws[Int].laws(using IntInstances.intMax))
  checkAll("Int[64]BitwiseTests", BitwiseOpsLaws[Int].laws(using IntInstances.int64))
}
