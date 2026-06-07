package com.nickrobison.inland.executor.instances.array

import com.nickrobison.inland.executor.{ArithOps, BitwiseOps, JSpecies, SimdVector, VectorBatch, VectorOps, toJVector}
import jdk.incubator.vector.{DoubleVector, IntVector, VectorMask, VectorSpecies}

inline given arrayVector[A]: VectorBatch[Array, A] with {
  def get(fa: Array[A], i: Int): A = fa(i)

  def set(fa: Array[A], i: Int, a: A): Unit = fa(i) = a

  def size(fa: Array[A]): Int = fa.length

  def isEmpty(fa: Array[A]): Boolean = size(fa) == 0
}

// Let's just try something here

private final class IntAlgebra(val species: VectorSpecies[Integer]) extends BitwiseOps[Int] {
  def and(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def or(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def xor(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

  def not(a: SimdVector[Int]): SimdVector[Int] = ???

  def shiftLeft(a: SimdVector[Int], n: Int): SimdVector[Int] = ???

  def shiftRight(a: SimdVector[Int], n: Int): SimdVector[Int] = ???

  def signedRightRigh(a: SimdVector[Int], n: Int): SimdVector[Int] = ???

  def plus(a: SimdVector[Int], b: SimdVector[Int]): SimdVector[Int] = ???

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

//  // TODO: This needs to be fully inlined
//  def fromVectorBatch[F[_]](fa: F[Int], offset: Int)(using VectorBatch[F, Int]): SimdVector[Int] = {
//    fa match {
//      case arr: Array[Int] => SimdVector[Int](toJVector(arr, offset)(using species))
//    }
//  }
//
//  def toVectorBatch[F[_]](v: SimdVector[Int], fa: F[Int], offset: Int)(using VectorBatch[F, Int]): Unit = ???

  def fromArray(arr: Array[Int], offset: Int): SimdVector[Int] = ???

  def toArray(v: SimdVector[Int], arr: Array[Int], offset: Int): Unit = ???
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

  def minus(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???

  def mult(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???

  def div(a: SimdVector[Double], b: SimdVector[Double]): SimdVector[Double] = ???

  def negate(a: SimdVector[Double]): SimdVector[Double] = ???

  def abs(a: SimdVector[Double]): SimdVector[Double] = ???

  def fma(a: SimdVector[Double], b: SimdVector[Double], c: SimdVector[Double]): SimdVector[Double] = ???

  def broadcast(e: Double): SimdVector[Double] = ???

  def zero: SimdVector[Double] = ???

  def reduceLanesAdd(v: SimdVector[Double]): Double = ???

  def blend(a: SimdVector[Double], b: SimdVector[Double], mask: VectorMask[Double]): SimdVector[Double] = ???

//  def fromVectorBatch[F[_]](fa: F[Double], offset: Int)(using VectorBatch[F, Double]): SimdVector[Double] = {
//
//  }
//
//  def toVectorBatch[F[_]](v: SimdVector[Double], fa: F[Double], offset: Int)(using VectorBatch[F, Double]): Unit = ???

  // TODO: This can be fully inlined into the parent trait, I think.
  def fromArray(arr: Array[Double], offset: Int): SimdVector[Double] = SimdVector(DoubleVector.fromArray(species, arr, offset))

  def toArray(v: SimdVector[Double], arr: Array[Double], offset: Int): Unit = {
    v.underlying.asInstanceOf[DoubleVector].intoArray(arr, offset)
  }
}