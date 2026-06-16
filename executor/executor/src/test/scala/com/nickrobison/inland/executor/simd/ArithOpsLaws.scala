package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.arith.*
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws
import spire.algebra.{Eq, Signed}

import scala.reflect.ClassTag

trait ArithOpsLaws[E: {ClassTag, Numeric}] extends Laws with LawInstances {

  given eqE: Eq[E] = scala.compiletime.deferred
  given signedE: Signed[E] = scala.compiletime.deferred
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
    "absolute is non-negative" -> forAll(absNonNegative),
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
    arrayEq(v.toSimd.toArray, v)
  }

  private def addCommutative(x: Array[E], y: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    arrayEq((xV + yV).toArray, (yV + xV).toArray)
  }

  private def addAssociative(x: Array[E], y: Array[E], z: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    val zV = z.toSimd
    arrayEq(((xV + yV) + zV).toArray, (xV + (yV + zV)).toArray)
  }

  private def addZero(x: Array[E])(using ops: ArithOps[E]) = {
    arrayEq((x.toSimd + ops.zero).toArray, x)
  }

  private def addInverse(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    arrayEq((xV + (-xV)).toArray, ops.zero.toArray)
  }

  private def multCommutative(x: Array[E], y: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    arrayEq((xV * yV).toArray, (yV * xV).toArray)
  }

  private def multOneIdentity(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    arrayEq((xV * ops.one).toArray, x)
  }

  private def multZeroAnhihilater(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    arrayEq((xV * ops.zero).toArray, ops.zero.toArray)
  }

  private def multDistributesOverAdds(x: Array[E], y: Array[E], z: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    val yV = y.toSimd
    val zV = z.toSimd
    val lhs = (xV * (yV + zV)).toArray
    val rhs = ((xV * yV) + (xV * zV)).toArray
    val ct = summon[ClassTag[E]]
    lhs.zip(rhs).forall { (l, r) =>
      if ct.runtimeClass == classOf[Double] then
        val ld = l.asInstanceOf[Double]
        val rd = r.asInstanceOf[Double]
        !ld.isFinite || !rd.isFinite || eqE.eqv(l, r)
      else eqE.eqv(l, r)
    }
  }

  private def absNonNegative(x: Array[E])(using ops: ArithOps[E]) = {
    val signed = summon[Signed[E]]
    ops.abs(x.toSimd).toArray.zip(x).forall { (v, orig) =>
      if orig == Int.MinValue.asInstanceOf[E] then true
      else !signed.isSignNegative(v)
    }
  }

  private def absIdempotent(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    arrayEq(ops.abs(ops.abs(xV)).toArray, ops.abs(xV).toArray)
  }

  private def absNegatesAbs(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    arrayEq(ops.abs(-xV).toArray, ops.abs(xV).toArray)
  }

  private def negateInvolution(x: Array[E])(using ops: ArithOps[E]) = {
    val xV = x.toSimd
    arrayEq((-(-xV)).toArray, x)
  }

  private def broadcastFillsAllLanes(x: E)(using ops: ArithOps[E]) = {
    ops.broadcast(x).toArray.forall(eqE.eqv(_, x))
  }

  private def reduceLanesEqualsScalar(x: Array[E])(using ops: ArithOps[E]) = {
    eqE.eqv(ops.reduceLanesAdd(x.toSimd), x.sum)
  }

}

object ArithOpsLaws extends LawInstances {
  def apply[E: {ClassTag, Numeric}](using Arbitrary[E], Eq[E], Signed[E]): ArithOpsLaws[E] = new ArithOpsLaws[E] {}
}
