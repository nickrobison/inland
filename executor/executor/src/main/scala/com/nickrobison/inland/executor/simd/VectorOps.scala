package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.VectorBatch
import jdk.incubator.vector.VectorMask

trait VectorOps[E] {

  def species: JSpecies[E]

  inline def lanes: Int = species.length()

  def broadcast(e: E): SimdVector[E]

  def zero: SimdVector[E]

  def reduceLanesAdd(v: SimdVector[E]): E

  def blend(a: SimdVector[E], b: SimdVector[E], mask: VectorMask[E]): SimdVector[E]

  def fromVectorBatch[F[_]](fa: F[E], offset: Int)(using
      vb: VectorBatch[F, E]): SimdVector[E]

  def toVectorBatch[F[_]](v: SimdVector[E], fa: F[E], offset: Int)(using
      vb: VectorBatch[F, E]): Unit

  def fromArray(arr: Array[E], offset: Int): SimdVector[E]
  def toArray(v: SimdVector[E], arr: Array[E], offset: Int): Unit

}
