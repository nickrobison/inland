package com.nickrobison.inland.executor.instances.array

import com.nickrobison.inland.executor.simd.{
  ArithOps,
  BitwiseOps,
  FloatOps,
  JSpecies,
  OrderOps,
  SimdVector,
  VectorOps,
  fromJVector,
  toJVector
}
import com.nickrobison.inland.executor.VectorBatch
import jdk.incubator.vector.{DoubleVector, FloatVector, IntVector, LongVector, ShortVector, VectorMask, VectorOperators, VectorSpecies}
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
  private[array] def forSpecies(sp: JSpecies[Double]): OrderOps[Double] & BitwiseOps[Double] & FloatOps[Double] =
    new DoubleAlgebra(sp)

  given double256: (OrderOps[Double] & BitwiseOps[Double] & FloatOps[Double]) = forSpecies(DoubleVector.SPECIES_256)
  given double512: (OrderOps[Double] & BitwiseOps[Double] & FloatOps[Double]) = forSpecies(DoubleVector.SPECIES_512)
}

private final class DoubleAlgebra(val species: JSpecies[Double]) extends BitwiseOps[Double] with FloatOps[Double] {

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
  def div(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.div(b.underlying))
  def negate(a: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.neg())
  def abs(a: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.abs())
  def fma(a: SimdVector[Double], b: SimdVector[Double], c: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.fma(b.underlying, c.underlying))

  def sqrt(a: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.lanewise(VectorOperators.SQRT))
  def reciprocal(a: SimdVector[Double]): SimdVector[Double] =
    div(broadcast(1.0), a)
  def sin(a: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.lanewise(VectorOperators.SIN))
  def cos(a: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.lanewise(VectorOperators.COS))
  def log(a: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.lanewise(VectorOperators.LOG))
  def exp(a: SimdVector[Double]): SimdVector[Double] =
    SimdVector(a.underlying.lanewise(VectorOperators.EXP))

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

object FloatInstances {
  private[array] def forSpecies(sp: JSpecies[Float]): OrderOps[Float] & BitwiseOps[Float] & FloatOps[Float] =
    new FloatAlgebra(sp)

  given float256: (OrderOps[Float] & BitwiseOps[Float] & FloatOps[Float]) = forSpecies(FloatVector.SPECIES_256)
  given float512: (OrderOps[Float] & BitwiseOps[Float] & FloatOps[Float]) = forSpecies(FloatVector.SPECIES_512)
}

private final class FloatAlgebra(val species: JSpecies[Float]) extends BitwiseOps[Float] with FloatOps[Float] {

  def and(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] = ???
  def or(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] = ???
  def xor(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] = ???
  def not(a: SimdVector[Float]): SimdVector[Float] = ???
  def shiftLeft(a: SimdVector[Float], n: Int): SimdVector[Float] = ???
  def shiftRight(a: SimdVector[Float], n: Int): SimdVector[Float] = ???
  def signedRightRigh(a: SimdVector[Float], n: Int): SimdVector[Float] = ???

  def plus(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.add(b.underlying))
  def minus(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.sub(b.underlying))
  def mult(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.mul(b.underlying))
  def div(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.div(b.underlying))
  def negate(a: SimdVector[Float]): SimdVector[Float] = SimdVector(a.underlying.neg())
  def abs(a: SimdVector[Float]): SimdVector[Float] = SimdVector(a.underlying.abs())
  def fma(a: SimdVector[Float], b: SimdVector[Float], c: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.fma(b.underlying, c.underlying))

  def sqrt(a: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.lanewise(VectorOperators.SQRT))
  def reciprocal(a: SimdVector[Float]): SimdVector[Float] =
    div(broadcast(1.0f), a)
  def sin(a: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.lanewise(VectorOperators.SIN))
  def cos(a: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.lanewise(VectorOperators.COS))
  def log(a: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.lanewise(VectorOperators.LOG))
  def exp(a: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.lanewise(VectorOperators.EXP))

  def lt(a: SimdVector[Float], b: SimdVector[Float]): VectorMask[Float] =
    a.underlying.compare(VectorOperators.LT, b.underlying).asInstanceOf[VectorMask[Float]]
  def lte(a: SimdVector[Float], b: SimdVector[Float]): VectorMask[Float] =
    a.underlying.compare(VectorOperators.LE, b.underlying).asInstanceOf[VectorMask[Float]]
  def gt(a: SimdVector[Float], b: SimdVector[Float]): VectorMask[Float] =
    a.underlying.compare(VectorOperators.GT, b.underlying).asInstanceOf[VectorMask[Float]]
  def gte(a: SimdVector[Float], b: SimdVector[Float]): VectorMask[Float] =
    a.underlying.compare(VectorOperators.GE, b.underlying).asInstanceOf[VectorMask[Float]]
  def min(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.min(b.underlying))
  def max(a: SimdVector[Float], b: SimdVector[Float]): SimdVector[Float] =
    SimdVector(a.underlying.max(b.underlying))
  def reduceLanesMin(a: SimdVector[Float]): Float =
    a.underlying.reduceLanes(VectorOperators.MIN)
  def reduceLanesMax(a: SimdVector[Float]): Float =
    a.underlying.reduceLanes(VectorOperators.MAX)

  inline def broadcast(e: Float): SimdVector[Float] = SimdVector(
    FloatVector.broadcast(species, e))
  def zero: SimdVector[Float] = broadcast(0)
  def one: SimdVector[Float] = broadcast(1.0f)

  def reduceLanesAdd(v: SimdVector[Float]): Float = v.underlying.reduceLanes(VectorOperators.ADD)
  def blend(
      a: SimdVector[Float],
      b: SimdVector[Float],
      mask: VectorMask[Float]): SimdVector[Float] = ???

  def fromArr(arr: Array[Float], offset: Int): SimdVector[Float] =
    SimdVector(toJVector(arr, offset)(using species))
  def toArray(v: SimdVector[Float], arr: Array[Float], offset: Int): Unit =
    v.underlying.intoArray(arr, offset)

  transparent inline def fromVectorBatch[F[_]](fa: F[Float], offset: Int)(using
      vb: VectorBatch[F, Float]): SimdVector[Float] = {
    inline fa match {
      case arr: Array[Float] => SimdVector(toJVector(arr, offset)(using species))
      case _ =>
        val scratch = new Array[Float](lanes)
        var i = 0
        while (i < lanes) {
          scratch(i) = vb.get(fa, offset + i)
          i += 1
        }
        SimdVector(toJVector(scratch, 0)(using species))
    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[Float], fa: F[Float], offset: Int)(
      using vb: VectorBatch[F, Float]): Unit = {
    inline fa match {
      case arr: Array[Float] => fromJVector[Float](v.underlying, arr, offset)(using species)
      case _ =>
        val scratch = new Array[Float](lanes)
        v.underlying.intoArray(scratch, 0)
        var i = 0
        while (i < lanes) {
          vb.set(fa, offset + i, scratch(i))
          i += 1
        }
    }
  }
}

object LongInstances {
  private[array] def forSpecies(sp: JSpecies[Long]): BitwiseOps[Long] & OrderOps[Long] = new LongAlgebra(sp)

  val long256: BitwiseOps[Long] & OrderOps[Long] = forSpecies(LongVector.SPECIES_256)
  val longPref: BitwiseOps[Long] & OrderOps[Long] = forSpecies(LongVector.SPECIES_PREFERRED)
  val longMax: BitwiseOps[Long] & OrderOps[Long] = forSpecies(LongVector.SPECIES_MAX)
}

private final class LongAlgebra(val species: VectorSpecies[java.lang.Long]) extends BitwiseOps[Long] {

  def and(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.and(b.underlying))
  def or(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.or(b.underlying))
  def xor(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.lanewise(VectorOperators.XOR, b.underlying))
  def not(a: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.lanewise(VectorOperators.NOT))
  def shiftLeft(a: SimdVector[Long], n: Int): SimdVector[Long] =
    SimdVector(a.underlying.lanewise(VectorOperators.LSHL, n))
  def shiftRight(a: SimdVector[Long], n: Int): SimdVector[Long] =
    SimdVector(a.underlying.lanewise(VectorOperators.LSHR, n))
  def signedRightRigh(a: SimdVector[Long], n: Int): SimdVector[Long] =
    SimdVector(a.underlying.lanewise(VectorOperators.ASHR, n))

  def plus(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.add(b.underlying))
  def minus(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.sub(b.underlying))
  def mult(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.mul(b.underlying))
  def div(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.div(b.underlying))
  def negate(a: SimdVector[Long]): SimdVector[Long] = SimdVector(a.underlying.neg())
  def abs(a: SimdVector[Long]): SimdVector[Long] = SimdVector(a.underlying.abs())
  def fma(a: SimdVector[Long], b: SimdVector[Long], c: SimdVector[Long]): SimdVector[Long] = ???

  def lt(a: SimdVector[Long], b: SimdVector[Long]): VectorMask[Long] =
    a.underlying.compare(VectorOperators.LT, b.underlying).asInstanceOf[VectorMask[Long]]
  def lte(a: SimdVector[Long], b: SimdVector[Long]): VectorMask[Long] =
    a.underlying.compare(VectorOperators.LE, b.underlying).asInstanceOf[VectorMask[Long]]
  def gt(a: SimdVector[Long], b: SimdVector[Long]): VectorMask[Long] =
    a.underlying.compare(VectorOperators.GT, b.underlying).asInstanceOf[VectorMask[Long]]
  def gte(a: SimdVector[Long], b: SimdVector[Long]): VectorMask[Long] =
    a.underlying.compare(VectorOperators.GE, b.underlying).asInstanceOf[VectorMask[Long]]
  def min(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.min(b.underlying))
  def max(a: SimdVector[Long], b: SimdVector[Long]): SimdVector[Long] =
    SimdVector(a.underlying.max(b.underlying))
  def reduceLanesMin(a: SimdVector[Long]): Long =
    a.underlying.reduceLanes(VectorOperators.MIN)
  def reduceLanesMax(a: SimdVector[Long]): Long =
    a.underlying.reduceLanes(VectorOperators.MAX)

  inline def broadcast(e: Long): SimdVector[Long] = SimdVector(LongVector.broadcast(species, e))
  def zero: SimdVector[Long] = broadcast(0L)
  def one: SimdVector[Long] = broadcast(1L)

  def reduceLanesAdd(v: SimdVector[Long]): Long = v.underlying.reduceLanes(VectorOperators.ADD)
  def blend(a: SimdVector[Long], b: SimdVector[Long], mask: VectorMask[Long]): SimdVector[Long] = ???

  def fromArr(arr: Array[Long], offset: Int): SimdVector[Long] = SimdVector(
    toJVector(arr, offset)(using species))
  def toArray(v: SimdVector[Long], arr: Array[Long], offset: Int): Unit =
    fromJVector[Long](v.underlying, arr, offset)(using species)

  transparent inline def fromVectorBatch[F[_]](fa: F[Long], offset: Int)(using
      vb: VectorBatch[F, Long]): SimdVector[Long] = {
    inline fa match {
      case arr: Array[Long] => SimdVector(toJVector(arr, offset)(using species))
      case _ =>
        val scratch = new Array[Long](lanes)
        var i = 0
        while (i < lanes) {
          scratch(i) = vb.get(fa, offset + i)
          i += 1
        }
        SimdVector(toJVector(scratch, 0)(using species))
    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[Long], fa: F[Long], offset: Int)(using
      vb: VectorBatch[F, Long]): Unit = {
    inline fa match {
      case arr: Array[Long] => fromJVector[Long](v.underlying, arr, offset)(using species)
      case _ =>
        val scratch = new Array[Long](lanes)
        v.underlying.intoArray(scratch, 0)
        var i = 0
        while (i < lanes) {
          vb.set(fa, offset + i, scratch(i))
          i += 1
        }
    }
  }
}

object ShortInstances {
  private[array] def forSpecies(sp: JSpecies[Short]): BitwiseOps[Short] & OrderOps[Short] = new ShortAlgebra(sp)

  val short128: BitwiseOps[Short] & OrderOps[Short] = forSpecies(ShortVector.SPECIES_128)
  val shortPref: BitwiseOps[Short] & OrderOps[Short] = forSpecies(ShortVector.SPECIES_PREFERRED)
  val shortMax: BitwiseOps[Short] & OrderOps[Short] = forSpecies(ShortVector.SPECIES_MAX)
}

private final class ShortAlgebra(val species: VectorSpecies[java.lang.Short]) extends BitwiseOps[Short] {

  def and(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.and(b.underlying))
  def or(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.or(b.underlying))
  def xor(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.lanewise(VectorOperators.XOR, b.underlying))
  def not(a: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.lanewise(VectorOperators.NOT))
  def shiftLeft(a: SimdVector[Short], n: Int): SimdVector[Short] =
    SimdVector(a.underlying.lanewise(VectorOperators.LSHL, n))
  def shiftRight(a: SimdVector[Short], n: Int): SimdVector[Short] =
    SimdVector(a.underlying.lanewise(VectorOperators.LSHR, n))
  def signedRightRigh(a: SimdVector[Short], n: Int): SimdVector[Short] =
    SimdVector(a.underlying.lanewise(VectorOperators.ASHR, n))

  def plus(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.add(b.underlying))
  def minus(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.sub(b.underlying))
  def mult(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.mul(b.underlying))
  def div(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.div(b.underlying))
  def negate(a: SimdVector[Short]): SimdVector[Short] = SimdVector(a.underlying.neg())
  def abs(a: SimdVector[Short]): SimdVector[Short] = SimdVector(a.underlying.abs())
  def fma(a: SimdVector[Short], b: SimdVector[Short], c: SimdVector[Short]): SimdVector[Short] = ???

  def lt(a: SimdVector[Short], b: SimdVector[Short]): VectorMask[Short] =
    a.underlying.compare(VectorOperators.LT, b.underlying).asInstanceOf[VectorMask[Short]]
  def lte(a: SimdVector[Short], b: SimdVector[Short]): VectorMask[Short] =
    a.underlying.compare(VectorOperators.LE, b.underlying).asInstanceOf[VectorMask[Short]]
  def gt(a: SimdVector[Short], b: SimdVector[Short]): VectorMask[Short] =
    a.underlying.compare(VectorOperators.GT, b.underlying).asInstanceOf[VectorMask[Short]]
  def gte(a: SimdVector[Short], b: SimdVector[Short]): VectorMask[Short] =
    a.underlying.compare(VectorOperators.GE, b.underlying).asInstanceOf[VectorMask[Short]]
  def min(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.min(b.underlying))
  def max(a: SimdVector[Short], b: SimdVector[Short]): SimdVector[Short] =
    SimdVector(a.underlying.max(b.underlying))
  def reduceLanesMin(a: SimdVector[Short]): Short =
    a.underlying.reduceLanes(VectorOperators.MIN)
  def reduceLanesMax(a: SimdVector[Short]): Short =
    a.underlying.reduceLanes(VectorOperators.MAX)

  inline def broadcast(e: Short): SimdVector[Short] = SimdVector(ShortVector.broadcast(species, e))
  def zero: SimdVector[Short] = broadcast(0.toShort)
  def one: SimdVector[Short] = broadcast(1.toShort)

  def reduceLanesAdd(v: SimdVector[Short]): Short = v.underlying.reduceLanes(VectorOperators.ADD)
  def blend(a: SimdVector[Short], b: SimdVector[Short], mask: VectorMask[Short]): SimdVector[Short] = ???

  def fromArr(arr: Array[Short], offset: Int): SimdVector[Short] = SimdVector(
    toJVector(arr, offset)(using species))
  def toArray(v: SimdVector[Short], arr: Array[Short], offset: Int): Unit =
    fromJVector[Short](v.underlying, arr, offset)(using species)

  transparent inline def fromVectorBatch[F[_]](fa: F[Short], offset: Int)(using
      vb: VectorBatch[F, Short]): SimdVector[Short] = {
    inline fa match {
      case arr: Array[Short] => SimdVector(toJVector(arr, offset)(using species))
      case _ =>
        val scratch = new Array[Short](lanes)
        var i = 0
        while (i < lanes) {
          scratch(i) = vb.get(fa, offset + i)
          i += 1
        }
        SimdVector(toJVector(scratch, 0)(using species))
    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[Short], fa: F[Short], offset: Int)(using
      vb: VectorBatch[F, Short]): Unit = {
    inline fa match {
      case arr: Array[Short] => fromJVector[Short](v.underlying, arr, offset)(using species)
      case _ =>
        val scratch = new Array[Short](lanes)
        v.underlying.intoArray(scratch, 0)
        var i = 0
        while (i < lanes) {
          vb.set(fa, offset + i, scratch(i))
          i += 1
        }
    }
  }
}
