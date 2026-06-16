package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.instances.array.{IntInstances, LongInstances, ShortInstances}
import com.nickrobison.inland.executor.simd.ArithOpsLaws.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class BitwiseAlgebraTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Int[Preferred]BitwiseTests", BitwiseOpsLaws[Int].laws(using IntInstances.intPreferred))
  checkAll("Int[Max]BitwiseTests", BitwiseOpsLaws[Int].laws(using IntInstances.intMax))
  checkAll("Int[64]BitwiseTests", BitwiseOpsLaws[Int].laws(using IntInstances.int64))
  checkAll("Long[256]BitwiseTests", BitwiseOpsLaws[Long].laws(using LongInstances.long256))
  checkAll("Long[Preferred]BitwiseTests", BitwiseOpsLaws[Long].laws(using LongInstances.longPref))
  checkAll("Long[Max]BitwiseTests", BitwiseOpsLaws[Long].laws(using LongInstances.longMax))
  checkAll("Short[128]BitwiseTests", BitwiseOpsLaws[Short].laws(using ShortInstances.short128))
  checkAll("Short[Preferred]BitwiseTests", BitwiseOpsLaws[Short].laws(using ShortInstances.shortPref))
  checkAll("Short[Max]BitwiseTests", BitwiseOpsLaws[Short].laws(using ShortInstances.shortMax))
}
