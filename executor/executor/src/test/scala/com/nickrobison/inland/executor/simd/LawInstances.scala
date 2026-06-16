package com.nickrobison.inland.executor.simd

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import spire.algebra.{Eq, Signed}
import spire.std.AnyInstances

import scala.reflect.ClassTag

trait LawInstances extends AnyInstances {

  given Eq[Int] with
    def eqv(x: Int, y: Int): Boolean = x == y

  given Eq[Double] with
    def eqv(x: Double, y: Double): Boolean =
      if x == y then true
      else if x.isNaN && y.isNaN then true
      else if !x.isFinite || !y.isFinite then false
      else Math.abs(x - y) / Math.max(Math.abs(x), Math.abs(y)) < 1e-12

  given Eq[Float] with
    def eqv(x: Float, y: Float): Boolean =
      if x == y then true
      else if x.isNaN && y.isNaN then true
      else if !x.isFinite || !y.isFinite then false
      else Math.abs(x - y) / Math.max(Math.abs(x), Math.abs(y)) < 1e-6f

  given Signed[Int] = new spire.std.IntAlgebra
  given Signed[Double] = new spire.std.DoubleAlgebra
  given Signed[Float] = new spire.std.FloatAlgebra

  extension [E](arr: Array[E])(using ops: VectorOps[E]) {
    def toSimd: SimdVector[E] =
      ops.fromArr(arr, 0)
  }

  extension [E: ClassTag](v: SimdVector[E])(using ops: VectorOps[E]) {
    def toArray: Array[E] = {
      val out = new Array[E](ops.lanes)
      ops.toArray(v, out, 0)
      out
    }
  }

  protected def arrayEq[E](a: Array[E], b: Array[E])(using eqE: Eq[E]): Boolean =
    a.length == b.length && a.indices.forall(i => eqE.eqv(a(i), b(i)))

  protected def genArray[E](lanes: Int)(using arbE: Arbitrary[E], ct: ClassTag[E]): Gen[Array[E]] =
    Gen.containerOfN[Array, E](lanes, arbE.arbitrary)
}
