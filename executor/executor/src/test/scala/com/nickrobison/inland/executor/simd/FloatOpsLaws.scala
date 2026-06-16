package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.arith.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import spire.algebra.{Eq, Signed}

import scala.reflect.ClassTag

trait FloatOpsLaws[E: {ClassTag, Numeric}] extends Laws with LawInstances {

  given eqE: Eq[E] = scala.compiletime.deferred
  given signedE: Signed[E] = scala.compiletime.deferred
  given arbE: Arbitrary[E] = scala.compiletime.deferred

  given arbArray(using ops: FloatOps[E]): Arbitrary[Array[E]] = Arbitrary(
    genArray(ops.lanes))

  def structure(using FloatOps[E]): RuleSet = new DefaultRuleSet(
    name = "structure",
    parent = None,
    "round-trip via fromArray/toArray" -> forAll(arrayRoundTrip)
  )

  def floatOps(using FloatOps[E]): RuleSet = new DefaultRuleSet(
    name = "floatOps",
    parent = Some(structure),
    "sqrt square is abs" -> forAll(sqrtSquareIsAbs),
    "reciprocal multiply is one" -> forAll(reciprocalMultiplyIsOne),
    "exp log inverse" -> forAll(expLogInverse),
    "sin squared plus cos squared is one" -> forAll(sinSqPlusCosSq)
  )

  def laws(using FloatOps[E]): RuleSet = new DefaultRuleSet(
    name = "FloatOps",
    parent = Some(floatOps)
  )

  private def arrayRoundTrip(v: Array[E])(using ops: FloatOps[E]) =
    arrayEq(v.toSimd.toArray, v)

  private def sqrtSquareIsAbs(x: Array[E])(using ops: FloatOps[E]) = {
    val xV = x.toSimd
    arrayEq(ops.sqrt(xV * xV).toArray, ops.abs(xV).toArray)
  }

  private def reciprocalMultiplyIsOne(x: Array[E])(using ops: FloatOps[E]) = {
    val xV = x.toSimd
    val prod = ops.reciprocal(xV) * xV
    val prodArr = prod.toArray
    val oneArr = ops.one.toArray
    val zeroArr = ops.zero.toArray
    x.indices.forall { i =>
      if eqE.eqv(x(i), zeroArr(i)) then true
      else eqE.eqv(prodArr(i), oneArr(i))
    }
  }

  private def expLogInverse(x: Array[E])(using ops: FloatOps[E]) = {
    val xV = x.toSimd
    val result = ops.log(ops.exp(xV))
    val resultArr = result.toArray
    val oneArr = ops.one.toArray
    x.indices.forall { i =>
      if eqE.eqv(x(i), oneArr(i)) then true
      else eqE.eqv(resultArr(i), x(i))
    }
  }

  private def sinSqPlusCosSq(x: Array[E])(using ops: FloatOps[E]) = {
    val xV = x.toSimd
    val sinV = ops.sin(xV)
    val cosV = ops.cos(xV)
    val sum = sinV * sinV + cosV * cosV
    val sumArr = sum.toArray
    val oneArr = ops.one.toArray
    val ct = summon[ClassTag[E]]
    x.indices.forall { i =>
      val s = sumArr(i)
      val shouldSkip = ct.runtimeClass match {
        case c if c == classOf[Double] => !s.asInstanceOf[Double].isFinite
        case c if c == classOf[Float] => !s.asInstanceOf[Float].isFinite
        case _ => false
      }
      shouldSkip || eqE.eqv(s, oneArr(i))
    }
  }
}

object FloatOpsLaws extends LawInstances {
  def apply[E: {ClassTag, Numeric}](using Arbitrary[E], Eq[E], Signed[E]): FloatOpsLaws[E] = new FloatOpsLaws[E] {}
}
