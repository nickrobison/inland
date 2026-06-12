package com.nickrobison.inland.executor.instances.array

import com.nickrobison.inland.executor.simd.{
  ArithOps,
  BitwiseOps,
  JSpecies,
  OrderOps,
  SimdVector,
  VectorOps,
  fromJVector,
  toJVector
}
import com.nickrobison.inland.executor.VectorBatch
import jdk.incubator.vector.{DoubleVector, IntVector, VectorMask, VectorOperators, VectorSpecies}
import scala.reflect.ClassTag

inline given arrayVector[A]: VectorBatch[Array, A] with {
  def get(fa: Array[A], i: Int): A = fa(i)
  def set(fa: Array[A], i: Int, a: A): Unit = fa(i) = a
  def size(fa: Array[A]): Int = fa.length
  def isEmpty(fa: Array[A]): Boolean = size(fa) == 0
  def make(arr: Array[A]): Array[A] = arr.clone()
}

given scalaVectorInstance[A](using ct: ClassTag[A]): VectorBatch[scala.Vector, A] with {
  def get(fa: scala.Vector[A], i: Int): A = fa(i)
  def set(fa: scala.Vector[A], i: Int, a: A): Unit =
    throw new UnsupportedOperationException("scala.Vector is immutable")
  def size(fa: scala.Vector[A]): Int = fa.length
  def isEmpty(fa: scala.Vector[A]): Boolean = fa.isEmpty
  def make(arr: Array[A]): scala.Vector[A] = scala.Vector.from(arr)
}

object IntInstances {
  private def forSpecies(sp: JSpecies[Int]): BitwiseOps[Int] & OrderOps[Int] = new IntAlgebra(sp)

  val intPreferred: BitwiseOps[Int] & OrderOps[Int] = forSpecies(IntVector.SPECIES_PREFERRED)
  val int64: BitwiseOps[Int] & OrderOps[Int] = forSpecies(IntVector.SPECIES_64)
  val intMax: BitwiseOps[Int] & OrderOps[Int] = forSpecies(IntVector.SPECIES_MAX)
}

private final class IntAlgebra(val species: VectorSpecies[Integer]) extends BitwiseOps[Int] {

  def and(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.and(b.underlying))
  def or(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.or(b.underlying))
  def xor(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.lanewise(VectorOperators.XOR, b.underlying))
  def not(a: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.lanewise(VectorOperators.NOT))
  def shiftLeft(a: SimdVector[Int], n: Int): SimdVector[Int] =
    SimdVector(a.underlying.lanewise(VectorOperators.LSHL, n))
  def shiftRight(a: SimdVector[Int], n: Int): SimdVector[Int] =
    SimdVector(a.underlying.lanewise(VectorOperators.LSHR, n))
  def signedRightRigh(a: SimdVector[Int], n: Int): SimdVector[Int] =
    SimdVector(a.underlying.lanewise(VectorOperators.ASHR, n))

  def plus(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.add(b.underlying))
  def minus(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.sub(b.underlying))
  def mult(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.mul(b.underlying))
  def div(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???
  def negate(a: SimdVector[Int]): SimdVector[Int] = SimdVector(a.underlying.neg())
  def abs(a: SimdVector[Int]): SimdVector[Int] = SimdVector(a.underlying.abs())
  def fma(a: SimdVector[Int], b: SimdVector[Int], c: SimdVector[Int]): SimdVector[Int] = ???

  def lt(a: SimdVector[Int], b: SimdVector[Int]): VectorMask[Int] =
    a.underlying.compare(VectorOperators.LT, b.underlying).asInstanceOf[VectorMask[Int]]
  def lte(a: SimdVector[Int], b: SimdVector[Int]): VectorMask[Int] =
    a.underlying.compare(VectorOperators.LE, b.underlying).asInstanceOf[VectorMask[Int]]
  def gt(a: SimdVector[Int], b: SimdVector[Int]): VectorMask[Int] =
    a.underlying.compare(VectorOperators.GT, b.underlying).asInstanceOf[VectorMask[Int]]
  def gte(a: SimdVector[Int], b: SimdVector[Int]): VectorMask[Int] =
    a.underlying.compare(VectorOperators.GE, b.underlying).asInstanceOf[VectorMask[Int]]
  def min(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.min(b.underlying))
  def max(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] =
    SimdVector(a.underlying.max(b.underlying))
  def reduceLanesMin(a: SimdVector[Int]): Int =
    a.underlying.reduceLanes(VectorOperators.MIN)
  def reduceLanesMax(a: SimdVector[Int]): Int =
    a.underlying.reduceLanes(VectorOperators.MAX)

  inline def broadcast(e: Int): SimdVector[Int] = SimdVector(IntVector.broadcast(species, e))
  def zero: SimdVector[Int] = broadcast(0)
  def one: SimdVector[Int] = broadcast(1)

  def reduceLanesAdd(v: SimdVector[Int]): Int = v.underlying.reduceLanes(VectorOperators.ADD)
  def blend(a: SimdVector[Int], b: SimdVector[Int], mask: VectorMask[Int]): SimdVector[Int] = ???

  def fromArr(arr: Array[Int], offset: Int): SimdVector[Int] = SimdVector(
    toJVector(arr, offset)(using species))
  def toArray(v: SimdVector[Int], arr: Array[Int], offset: Int): Unit =
    fromJVector[Int](v.underlying, arr, offset)(using species)

  transparent inline def fromVectorBatch[F[_]](fa: F[Int], offset: Int)(using
      vb: VectorBatch[F, Int]): SimdVector[Int] = {
    inline fa match {
      case arr: Array[Int] => SimdVector(toJVector(arr, offset)(using species))
      case _ =>
        val scratch = new Array[Int](lanes)
        var i = 0
        while (i < lanes) {
          scratch(i) = vb.get(fa, offset + i)
          i += 1
        }
        SimdVector(toJVector(scratch, 0)(using species))
    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[Int], fa: F[Int], offset: Int)(using
      vb: VectorBatch[F, Int]): Unit = {
    inline fa match {
      case arr: Array[Int] => fromJVector[Int](v.underlying, arr, offset)(using species)
      case _ =>
        val scratch = new Array[Int](lanes)
        v.underlying.intoArray(scratch, 0)
        var i = 0
        while (i < lanes) {
          vb.set(fa, offset + i, scratch(i))
          i += 1
        }
    }
  }
}

object DoubleInstances {
  private def forSpecies(sp: JSpecies[Double]): OrderOps[Double] & BitwiseOps[Double] =
    new DoubleAlgebra(sp)

  given double256: (OrderOps[Double] & BitwiseOps[Double]) = forSpecies(DoubleVector.SPECIES_256)
  given double512: (OrderOps[Double] & BitwiseOps[Double]) = forSpecies(DoubleVector.SPECIES_512)
}

private final class DoubleAlgebra(val species: JSpecies[Double]) extends BitwiseOps[Double] {

  def and(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???
  def or(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???
  def xor(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???
  def not(a: SimdVector[Double]): SimdVector[Double] = ???
  def shiftLeft(a: SimdVector[Double], n: Int): SimdVector[Double] = ???
  def shiftRight(a: SimdVector[Double], n: Int): SimdVector[Double] = ???
  def signedRightRigh(a: SimdVector[Double], n: Int): SimdVector[Double] = ???

  def plus(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.add(b.underlying))
  def minus(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.sub(b.underlying))
  def mult(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.mul(b.underlying))
  def div(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???
  def negate(a: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.neg())
  def abs(a: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.abs())
  def fma(a: SimdVector[Double], b: SimdVector[Double], c: SimdVector[Double]): SimdVector[Double] =
    ???

  def lt(a: SimdVector[Double], b: SimdVector[Double]): VectorMask[Double] =
    a.underlying.compare(VectorOperators.LT, b.underlying).asInstanceOf[VectorMask[Double]]
  def lte(a: SimdVector[Double], b: SimdVector[Double]): VectorMask[Double] =
    a.underlying.compare(VectorOperators.LE, b.underlying).asInstanceOf[VectorMask[Double]]
  def gt(a: SimdVector[Double], b: SimdVector[Double]): VectorMask[Double] =
    a.underlying.compare(VectorOperators.GT, b.underlying).asInstanceOf[VectorMask[Double]]
  def gte(a: SimdVector[Double], b: SimdVector[Double]): VectorMask[Double] =
    a.underlying.compare(VectorOperators.GE, b.underlying).asInstanceOf[VectorMask[Double]]
  def min(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.min(b.underlying))
  def max(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.max(b.underlying))
  def reduceLanesMin(a: SimdVector[Double]): Double =
    a.underlying.reduceLanes(VectorOperators.MIN)
  def reduceLanesMax(a: SimdVector[Double]): Double =
    a.underlying.reduceLanes(VectorOperators.MAX)

  inline def broadcast(e: Double): SimdVector[Double] = SimdVector(
    DoubleVector.broadcast(species, e))
  def zero: SimdVector[Double] = broadcast(0)
  def one: SimdVector[Double] = broadcast(1.0)

  def reduceLanesAdd(v: SimdVector[Double]): Double = v.underlying.reduceLanes(VectorOperators.ADD)
  def blend(
      a: SimdVector[Double],
      b: SimdVector[Double],
      mask: VectorMask[Double]): SimdVector[Double] = ???

  def fromArr(arr: Array[Double], offset: Int): SimdVector[Double] =
    SimdVector(toJVector(arr, offset)(using species))
  def toArray(v: SimdVector[Double], arr: Array[Double], offset: Int): Unit =
    v.underlying.intoArray(arr, offset)

  transparent inline def fromVectorBatch[F[_]](fa: F[Double], offset: Int)(using
      vb: VectorBatch[F, Double]): SimdVector[Double] = {
    inline fa match {
      case arr: Array[Double] => SimdVector(toJVector(arr, offset)(using species))
      case _ =>
        val scratch = new Array[Double](lanes)
        var i = 0
        while (i < lanes) {
          scratch(i) = vb.get(fa, offset + i)
          i += 1
        }
        SimdVector(toJVector(scratch, 0)(using species))
    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[Double], fa: F[Double], offset: Int)(
      using vb: VectorBatch[F, Double]): Unit = {
    inline fa match {
      case arr: Array[Double] => fromJVector[Double](v.underlying, arr, offset)(using species)
      case _ =>
        val scratch = new Array[Double](lanes)
        v.underlying.intoArray(scratch, 0)
        var i = 0
        while (i < lanes) {
          vb.set(fa, offset + i, scratch(i))
          i += 1
        }
    }
  }
}
