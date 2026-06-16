package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.bitwise.*
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws
import spire.algebra.{Eq, Signed}

import scala.reflect.ClassTag

trait BitwiseOpsLaws[E: {ClassTag, Numeric}] extends Laws with LawInstances {

  given eqE: Eq[E] = scala.compiletime.deferred
  given signedE: Signed[E] = scala.compiletime.deferred
  given arbE: Arbitrary[E] = scala.compiletime.deferred

  given arbArray(using ops: BitwiseOps[E]): Arbitrary[Array[E]] = Arbitrary(
    genArray(ops.lanes))

  def structure(using BitwiseOps[E]): RuleSet = new DefaultRuleSet(
    name = "structure",
    parent = None,
    "round-trip via fromArray/toArray" -> forAll(arrayRoundTrip)
  )

  def bitwise(using BitwiseOps[E]): RuleSet = new DefaultRuleSet(
    name = "bitwise",
    parent = Some(structure),
    "and identity" -> forAll(andIdentity),
    "or identity" -> forAll(orIdentity),
    "xor inverse" -> forAll(xorInverse),
    "not involution" -> forAll(notInvolution),
    "and commutative" -> forAll(andCommutative),
    "or commutative" -> forAll(orCommutative),
    "xor commutative" -> forAll(xorCommutative),
    "de morgan and" -> forAll(deMorganAnd),
    "de morgan or" -> forAll(deMorganOr)
  )

  def shift(using BitwiseOps[E]): RuleSet = new DefaultRuleSet(
    name = "shift",
    parent = Some(bitwise),
    "shift left by 0 is identity" -> forAll(shiftLeftIdentity),
    "shift right by 0 is identity" -> forAll(shiftRightIdentity),
    "signed right shift by 0 is identity" -> forAll(signedRightShiftIdentity),
    "shift right then left recovers truncated" -> forAll(shiftRightLeft)
  )

  def laws(using BitwiseOps[E]): RuleSet = new DefaultRuleSet(
    name = "BitwiseOps",
    parent = Some(shift)
  )

  private def arrayRoundTrip(v: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq(v.toSimd.toArray, v)

  private def andIdentity(x: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq((x.toSimd & ops.zero).toArray, ops.zero.toArray)

  private def orIdentity(x: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq((x.toSimd | ops.zero).toArray, x)

  private def xorInverse(x: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq((x.toSimd ^ x.toSimd).toArray, ops.zero.toArray)

  private def notInvolution(x: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq((~(~(x.toSimd))).toArray, x)

  private def andCommutative(x: Array[E], y: Array[E])(using ops: BitwiseOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    arrayEq((xV & yV).toArray, (yV & xV).toArray)
  }

  private def orCommutative(x: Array[E], y: Array[E])(using ops: BitwiseOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    arrayEq((xV | yV).toArray, (yV | xV).toArray)
  }

  private def xorCommutative(x: Array[E], y: Array[E])(using ops: BitwiseOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    arrayEq((xV ^ yV).toArray, (yV ^ xV).toArray)
  }

  private def deMorganAnd(x: Array[E], y: Array[E])(using ops: BitwiseOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    val lhs = ~(xV & yV)
    val rhs = (~xV) | (~yV)
    arrayEq(lhs.toArray, rhs.toArray)
  }

  private def deMorganOr(x: Array[E], y: Array[E])(using ops: BitwiseOps[E]): Boolean = {
    val xV = x.toSimd
    val yV = y.toSimd
    val lhs = ~(xV | yV)
    val rhs = (~xV) & (~yV)
    arrayEq(lhs.toArray, rhs.toArray)
  }

  private def shiftLeftIdentity(x: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq((x.toSimd << 0).toArray, x)

  private def shiftRightIdentity(x: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq((x.toSimd >> 0).toArray, x)

  private def signedRightShiftIdentity(x: Array[E])(using ops: BitwiseOps[E]): Boolean =
    arrayEq(ops.signedRightRigh(x.toSimd, 0).toArray, x)

  private def shiftRightLeft(x: Array[E])(using ops: BitwiseOps[E]): Boolean = {
    val shifted = (x.toSimd >> 8) << 8
    val ct = summon[ClassTag[E]]
    val xArr = x
    val sArr = shifted.toArray
    var i = 0
    while (i < ops.lanes) {
      val expected = if ct.runtimeClass == classOf[Int] then
        (xArr(i).asInstanceOf[Int] >>> 8) << 8
      else xArr(i)
      if !eqE.eqv(sArr(i), expected.asInstanceOf[E]) then return false
      i += 1
    }
    true
  }
}

object BitwiseOpsLaws extends LawInstances {
  def apply[E: {ClassTag, Numeric}](using Arbitrary[E], Eq[E], Signed[E]): BitwiseOpsLaws[E] = new BitwiseOpsLaws[E] {}
}
