package com.nickrobison.inland.executor.instances.array

import com.nickrobison.inland.executor.simd.{ArithOps, BitwiseOps, JSpecies, SimdVector, VectorOps, fromJVector, toJVector}
import com.nickrobison.inland.executor.VectorBatch
import jdk.incubator.vector.{DoubleVector, IntVector, VectorMask, VectorSpecies}

inline given arrayVector[A]: VectorBatch[Array, A] with {
  def get(fa: Array[A], i: Int): A = fa(i)

  def set(fa: Array[A], i: Int, a: A): Unit = fa(i) = a

  def size(fa: Array[A]): Int = fa.length

  def isEmpty(fa: Array[A]): Boolean = size(fa) == 0
}

// Let's just try somcething here

object IntInstances {
  private def forSpecies(sp: JSpecies[Int]): BitwiseOps[Int] = new IntAlgebra(sp)

  val intPreferred: BitwiseOps[Int] = forSpecies(IntVector.SPECIES_PREFERRED)
  val int64: BitwiseOps[Int] = forSpecies(IntVector.SPECIES_64)
}

private final class IntAlgebra(val species: VectorSpecies[Integer]) extends BitwiseOps[Int] {
  def and(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def or(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def xor(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def not(a: SimdVector[Int]): SimdVector[Int] = ???

  def shiftLeft(a: SimdVector[Int], n: Int): SimdVector[Int] = ???

  def shiftRight(a: SimdVector[Int], n: Int): SimdVector[Int] = ???

  def signedRightRigh(a: SimdVector[Int], n: Int): SimdVector[Int] = ???

  def plus(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = SimdVector(a.underlying.add(b.underlying))

  def minus(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def mult(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def div(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def negate(a: SimdVector[Int]): SimdVector[Int] = ???

  def abs(a: SimdVector[Int]): SimdVector[Int] = ???

  def fma(a: SimdVector[Int], b: SimdVector[Int], c: SimdVector[Int]): SimdVector[Int] = ???

  def broadcast(e: Int): SimdVector[Int] = ???

  def zero: SimdVector[Int] = ???

  def reduceLanesAdd(v: SimdVector[Int]): Int = ???

  def blend(a: SimdVector[Int], b: SimdVector[Int], mask: VectorMask[Int]): SimdVector[Int] = ???

  def fromArray(arr: Array[Int], offset: Int): SimdVector[Int] = SimdVector(toJVector(arr, offset)(using species))

  def toArray(v: SimdVector[Int], arr: Array[Int], offset: Int): Unit = {
    fromJVector[Int](v.underlying, arr, offset)(using species)
  }

  // TODO: This could be a macro
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
        SimdVector(toJVector(scratch, offset)(using species))

    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[Int], fa: F[Int], offset: Int)(using
                                                                                                vb: VectorBatch[F, Int]): Unit = {
    inline fa match {
      case arr: Array[Int] => fromJVector[Int](v.underlying, arr, offset)(using species)
      case _ =>
        val scratch = new Array[Int](lanes)
        var i = 0
        while (i < lanes) {
          vb.set(fa, offset + 1, scratch(i))
          i += 1
        }
    }
  }
}

object DoubleInstances {

  private def forSpecies(sp: JSpecies[Double]): BitwiseOps[Double] = new DoubleAlgebra(sp)

  given double256: BitwiseOps[Double] = forSpecies(DoubleVector.SPECIES_256)
}

private final class DoubleAlgebra(val species: JSpecies[Double]) extends BitwiseOps[Double] {
  def and(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???

  def or(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???

  def xor(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???

  def not(a: SimdVector[Double]): SimdVector[Double] = ???

  def shiftLeft(a: SimdVector[Double], n: Int): SimdVector[Double] = ???

  def shiftRight(a: SimdVector[Double], n: Int): SimdVector[Double] = ???

  def signedRightRigh(a: SimdVector[Double], n: Int): SimdVector[Double] = ???

  def plus(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.add(b.underlying))

  def minus(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.sub(b.underlying))

  def mult(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.mul(b.underlying))

  def div(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???

  def negate(a: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.neg())

  def abs(a: SimdVector[Double]): SimdVector[Double] = SimdVector(a.underlying.abs())

  // TODO: How?
  def fma(a: SimdVector[Double], b: SimdVector[Double], c: SimdVector[Double]): SimdVector[Double] = ???

  def broadcast(e: Double): SimdVector[Double] = SimdVector(DoubleVector.broadcast(species, e))

  def zero: SimdVector[Double] = broadcast(0)

  // TODO: Can we do this with double vectors?
  def reduceLanesAdd(v: SimdVector[Double]): Double = ???

  def blend(a: SimdVector[Double], b: SimdVector[Double], mask: VectorMask[Double]): SimdVector[Double] = ???

  // TODO: This can be fully inlined into the parent trait, I think.
  def fromArray(arr: Array[Double], offset: Int): SimdVector[Double] = SimdVector(DoubleVector.fromArray(species, arr, offset))

  def toArray(v: SimdVector[Double], arr: Array[Double], offset: Int): Unit = {
    v.underlying.asInstanceOf[DoubleVector].intoArray(arr, offset)
  }



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
        SimdVector(toJVector(scratch, offset)(using species))

    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[Double], fa: F[Double], offset: Int)(using
                                                                                      vb: VectorBatch[F, Double]): Unit = {
    inline fa match {
      case arr: Array[Double] => fromJVector[Double](v.underlying, arr, offset)(using species)
      case _ =>
        val scratch = new Array[Double](lanes)
        var i = 0
        while (i < lanes) {
          vb.set(fa, offset + 1, scratch(i))
          i += 1
        }
    }
  }
}