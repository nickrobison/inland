package com.nickrobison.inland.executor.simd

import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import spire.algebra.{Eq, Signed}

import scala.reflect.ClassTag

trait OrderOpsLaws[E: {ClassTag, Numeric}] extends Laws with LawInstances {

  given eqE: Eq[E] = scala.compiletime.deferred
  given signedE: Signed[E] = scala.compiletime.deferred
  given arbE: Arbitrary[E] = scala.compiletime.deferred

  given arbArray(using ops: OrderOps[E]): Arbitrary[Array[E]] = Arbitrary(
    genArray(ops.lanes))

  def structure(using OrderOps[E]): RuleSet = new DefaultRuleSet(
    name = "structure",
    parent = None,
    "round-trip via fromArray/toArray" -> forAll(arrayRoundTrip)
  )

  def comparison(using OrderOps[E]): RuleSet = new DefaultRuleSet(
    name = "comparison",
    parent = Some(structure),
    "min matches scalar lt" -> forAll(minMatchesScalar),
    "max matches scalar gt" -> forAll(maxMatchesScalar),
    "lt and gte are complementary" -> forAll(ltGteComplementary),
    "lte and gt are complementary" -> forAll(lteGtComplementary)
  )

  def ordering(using OrderOps[E]): RuleSet = new DefaultRuleSet(
    name = "ordering",
    parent = Some(comparison),
    "min is commutative" -> forAll(minCommutative),
    "min is idempotent" -> forAll(minIdempotent),
    "max is commutative" -> forAll(maxCommutative),
    "max is idempotent" -> forAll(maxIdempotent),
    "reduceLanesMin matches scalar min" -> forAll(reduceLanesMinMatchesScalar),
    "reduceLanesMax matches scalar max" -> forAll(reduceLanesMaxMatchesScalar)
  )

  def laws(using OrderOps[E]): RuleSet = new DefaultRuleSet(
    name = "OrderOps",
    parent = Some(ordering)
  )

  private def arrayRoundTrip(v: Array[E])(using ops: OrderOps[E]): Boolean =
    arrayEq(v.toSimd.toArray, v)

  private def minMatchesScalar(x: Array[E], y: Array[E])(using ops: OrderOps[E]): Boolean = {
    val num = summon[scala.math.Numeric[E]]
    val minV = ops.min(x.toSimd, y.toSimd).toArray
    val len = math.min(x.length, y.length)
    var i = 0
    while (i < len) {
      val expected = if (num.lt(x(i), y(i))) x(i) else y(i)
      if (minV(i) != expected) return false
      i += 1
    }
    true
  }

  private def maxMatchesScalar(x: Array[E], y: Array[E])(using ops: OrderOps[E]): Boolean = {
    val num = summon[scala.math.Numeric[E]]
    val maxV = ops.max(x.toSimd, y.toSimd).toArray
    val len = math.min(x.length, y.length)
    var i = 0
    while (i < len) {
      val expected = if (num.gt(x(i), y(i))) x(i) else y(i)
      if (maxV(i) != expected) return false
      i += 1
    }
    true
  }

  private def ltGteComplementary(x: Array[E], y: Array[E])(using ops: OrderOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    val minV = ops.min(xV, yV)
    val maxV = ops.max(xV, yV)
    arrayEq(ops.min(minV, maxV).toArray, minV.toArray) &&
    arrayEq(ops.max(minV, maxV).toArray, maxV.toArray)
  }

  private def lteGtComplementary(x: Array[E], y: Array[E])(using ops: OrderOps[E]): Boolean =
    ltGteComplementary(x, y)

  private def minCommutative(x: Array[E], y: Array[E])(using ops: OrderOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    arrayEq(ops.min(xV, yV).toArray, ops.min(yV, xV).toArray)
  }

  private def minIdempotent(x: Array[E])(using ops: OrderOps[E]): Boolean =
    arrayEq(ops.min(x.toSimd, x.toSimd).toArray, x)

  private def maxCommutative(x: Array[E], y: Array[E])(using ops: OrderOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    arrayEq(ops.max(xV, yV).toArray, ops.max(yV, xV).toArray)
  }

  private def maxIdempotent(x: Array[E])(using ops: OrderOps[E]): Boolean =
    arrayEq(ops.max(x.toSimd, x.toSimd).toArray, x)

  private def reduceLanesMinMatchesScalar(x: Array[E])(using ops: OrderOps[E]): Boolean = {
    eqE.eqv(ops.reduceLanesMin(x.toSimd), x.min)
  }

  private def reduceLanesMaxMatchesScalar(x: Array[E])(using ops: OrderOps[E]): Boolean = {
    eqE.eqv(ops.reduceLanesMax(x.toSimd), x.max)
  }
}

object OrderOpsLaws extends LawInstances {
  def apply[E: {ClassTag, Numeric}](using Arbitrary[E], Eq[E], Signed[E]): OrderOpsLaws[E] = new OrderOpsLaws[E] {}
}
