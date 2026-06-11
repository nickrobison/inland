package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.simd.ArithOpsLaws.{toArray, toSimd}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws

import scala.reflect.ClassTag

trait ArithOpsLaws[E: {ClassTag, Numeric}] extends Laws {

  given arbE: Arbitrary[E] = scala.compiletime.deferred

  given arbArray(using ops: ArithOps[E]): Arbitrary[Array[E]] = Arbitrary(
    Gen.containerOfN[Array, E](ops.lanes, arbE.arbitrary))

  def structure(using ArithOps[E]): RuleSet = new DefaultRuleSet(
    name = "structure",
    parent = None,
    "round-trip via fromArray/toArray" -> forAll(arrayRoundTrip)
  )

  def addition(using ArithOps[E]): RuleSet = new DefaultRuleSet(
    name = "addition",
    parent = Some(structure),
    "addition is commutative" -> forAll(addCommutative),
    "addition is associative" -> forAll(addAssociative),
    "add zero identity" -> forAll(addZero),
    "add inverse" -> forAll(addInverse)
  )

  def absolute(using ArithOps[E]): RuleSet = new DefaultRuleSet(
    name = "absolute",
    parent = Some(addition),
    "absolute is idempotent" -> forAll(absIdempotent),
    "absolute -> negate -> absolute equality" -> forAll(absNegatesAbs),
    "negation involution" -> forAll(negateInvolution)
  )

  def multiplication(using ArithOps[E]): RuleSet = new DefaultRuleSet(
    name = "multiplication",
    parent = Some(absolute),
    "multiplication is commutative" -> forAll(multCommutative),
    "multiplication by one is identity" -> forAll(multOneIdentity),
    "multiplication by zero is annihilator" -> forAll(multZeroAnhihilater),
    "multiplication distributes over addition" -> forAll(multDistributesOverAdds)
  )

  def lanewise(using ArithOps[E]): RuleSet  = new DefaultRuleSet(
    name = "lanewise",
    parent = Some(multiplication),
    "broadcast fills all lanes" -> forAll(broadcastFillsAllLanes),
    "reduce lanes by addition equals scalar" -> forAll(reduceLanesEqualsScalar)
  )

  def laws(using ArithOps[E]): RuleSet = new DefaultRuleSet(
    name = "ArithOps",
    parent = Some(lanewise)
  )

  private def arrayRoundTrip(v: Array[E])(using ops: ArithOps[E]) = {
    v.toSimd.toArray sameElements v
  }

  private def addCommutative(x: Array[E], y: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    ops.plus(xV, yV).toArray sameElements ops.plus(yV, xV).toArray
  }

  private def addAssociative(x: Array[E], y: Array[E], z: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    val zV = z.toSimd
    ops.plus(ops.plus(xV, yV), zV).toArray sameElements ops.plus(xV, ops.plus(yV, zV)).toArray
  }

  private def addZero(x: Array[E])(using ops: ArithOps[E]) = {
    ops.plus(x.toSimd, ops.zero).toArray sameElements x
  }

  private def addInverse(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    ops.plus(xV, ops.negate(xV)).toArray sameElements ops.zero.toArray
  }

  private def multCommutative(x: Array[E], y: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    ops.mult(xV, yV).toArray sameElements ops.mult(yV, xV).toArray
  }

  private def multOneIdentity(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    ops.mult(xV, ops.one).toArray sameElements x
  }

  private def multZeroAnhihilater(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    ops.mult(xV, ops.zero).toArray sameElements ops.zero.toArray
  }

  private def multDistributesOverAdds(x: Array[E], y: Array[E], z: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    val zV = z.toSimd
    ops.mult(xV, ops.plus(yV, zV)).toArray sameElements ops.plus(ops.mult(xV, yV), ops.mult(xV, zV)).toArray
  }

  // TODO: We need better comparisons here
//  private def absNonNegative(x: Array[E])(using ops: ArithOps[E]) = {
//    val xV = x.toSimd
//    ops.abs(x)
//  }

  private def absIdempotent(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    ops.abs(ops.abs(xV)).toArray sameElements ops.abs(xV).toArray
  }

  private def absNegatesAbs(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    ops.abs(ops.negate(xV)).toArray sameElements ops.abs(xV).toArray
  }

  private def negateInvolution(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    ops.negate(ops.negate(xV)).toArray sameElements x
  }

  private def broadcastFillsAllLanes(x: E)(using ops: ArithOps[E]) = {
    ops.broadcast(x).toArray.forall(_ == x)
  }

  private def reduceLanesEqualsScalar(x: Array[E])(using ops: ArithOps[E]) = {
    ops.reduceLanesAdd(x.toSimd) == x.sum
  }

}

object ArithOpsLaws {
  def apply[E: {ClassTag, Numeric}](using Arbitrary[E]): ArithOpsLaws[E] = new ArithOpsLaws[E] {}

  extension [E](arr: Array[E])(using ops: ArithOps[E]) {
    def toSimd: SimdVector[E] = {
      ops.fromArr(arr, 0)
    }
  }

  extension [E: ClassTag](v: SimdVector[E])(using ops: ArithOps[E]) {
    def toArray: Array[E] = {
      val out = new Array[E](ops.lanes)
      ops.toArray(v, out, 0)
      out
    }
  }
}
