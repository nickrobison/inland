package com.nickrobison.inland.executor.simd

import jdk.incubator.vector.VectorMask


trait OrderOps[F[_], E] extends VectorOps[E] {

  def lt(a: SimdVector[E], b: SimdVector[E]): VectorMask[E]
  def lte(a: SimdVector[E], b: SimdVector[E]): VectorMask[E]
  def gt(a: SimdVector[E], b: SimdVector[E]): VectorMask[E]
  def gte(a: SimdVector[E], b: SimdVector[E]): VectorMask[E]
  def min(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def max(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def reduceLanesMin(a: SimdVector[E]): E
  def reduceLanesMax(a: SimdVector[E]): E

}
