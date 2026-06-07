//package com.nickrobison.inland.executor
//
//import org.scalacheck.{Arbitrary, Gen}
//import org.scalatest.funsuite.AnyFunSuite
//import org.scalatestplus.scalacheck.Checkers
//import org.typelevel.discipline.scalatest.FunSuiteDiscipline
//import com.nickrobison.inland.executor.instances.array.given
//
//class ArrayTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {
//
//  given Arbitrary[Array[Int]] = Arbitrary(
//    for {
//      n <- Gen.choose(0, 1000)
//      elements <- Gen.listOfN(n, Gen.chooseNum(-128, 128))
//    } yield elements.toArray
//  )
//
//  given Arbitrary[VectorIdx] = Arbitrary(Gen.chooseNum(0, 100).map(VectorIdx.apply))
//
//  checkAll("VectorBatchLaws", VectorBatchLaws[Array, Int].laws)
//  checkAll("ArithOpsLaws", ArithOpsLaws[Array, Int].laws)
//}
