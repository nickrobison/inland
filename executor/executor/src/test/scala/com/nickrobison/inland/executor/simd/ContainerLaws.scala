package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.VectorBatch
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws
import spire.algebra.{Eq, Signed}

import scala.reflect.ClassTag

trait ContainerLaws[F[_], E: {ClassTag, Numeric}] extends Laws with LawInstances {

  given eqE: Eq[E] = scala.compiletime.deferred
  given signedE: Signed[E] = scala.compiletime.deferred
  given arbE: Arbitrary[E] = scala.compiletime.deferred

  private val paddedLanes: Int => Int = l => l + 3

  given arbArray(using ops: VectorOps[E]): Arbitrary[Array[E]] = Arbitrary(
    genArray(paddedLanes(ops.lanes)))

  private def minLength(using ops: VectorOps[E]): Int = paddedLanes(ops.lanes)

  def structure(using VectorOps[E]): RuleSet = new DefaultRuleSet(
    name = "structure",
    parent = None,
    "round-trip via fromArray/toArray" -> forAll(arrayRoundTrip)
  )

  def batchRead(using VectorOps[E], VectorBatch[F, E]): RuleSet = new DefaultRuleSet(
    name = "batchRead",
    parent = Some(structure),
    "fromVectorBatch equals fromArr" -> forAll(batchEqualsFromArr),
    "fromVectorBatch offset matches sliced fromArr" -> forAll(batchOffsetMatchesSlice)
  )

  def batchWrite(using VectorOps[E], VectorBatch[F, E]): RuleSet = new DefaultRuleSet(
    name = "batchWrite",
    parent = Some(batchRead),
    "fromVectorBatch round-trip via toVectorBatch" -> forAll(batchRoundTrip)
  )

  def laws(using VectorOps[E], VectorBatch[F, E]): RuleSet = new DefaultRuleSet(
    name = "Container",
    parent = Some(batchWrite)
  )

  private def arrayRoundTrip(arr: Array[E])(using ops: VectorOps[E]): Boolean =
    arrayEq(arr.take(ops.lanes).toSimd.toArray, arr.take(ops.lanes))

  private def batchRoundTrip(arr: Array[E])(using ops: VectorOps[E], vb: VectorBatch[F, E]): Boolean = {
    if (arr.length < ops.lanes) return true
    val v = ops.fromVectorBatch[F](vb.make(arr), 0)
    val out = new Array[E](ops.lanes)
    val outContainer = vb.make(out)
    ops.toVectorBatch(v, outContainer, 0)
    var i = 0
    while (i < ops.lanes) {
      out(i) = vb.get(outContainer, i)
      i += 1
    }
    arrayEq(out, arr.take(ops.lanes))
  }

  private def batchEqualsFromArr(arr: Array[E])(using ops: VectorOps[E], vb: VectorBatch[F, E]): Boolean = {
    if (arr.length < ops.lanes) return true
    val fromBatch = ops.fromVectorBatch[F](vb.make(arr), 0)
    val fromArr = ops.fromArr(arr, 0)
    arrayEq(fromBatch.toArray, fromArr.toArray)
  }

  private def batchOffsetMatchesSlice(arr: Array[E])(using ops: VectorOps[E], vb: VectorBatch[F, E]): Boolean = {
    val offset = 2
    if (arr.length < offset + ops.lanes) return true
    val fromBatch = ops.fromVectorBatch[F](vb.make(arr), offset)
    val fromArr = ops.fromArr(arr, offset)
    arrayEq(fromBatch.toArray, fromArr.toArray)
  }
}

object ContainerLaws extends LawInstances {
  def apply[F[_], E: {ClassTag, Numeric}](using Arbitrary[E], Eq[E], Signed[E]): ContainerLaws[F, E] = new ContainerLaws[F, E] {}
}
