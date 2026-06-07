package com.nickrobison.inland.executor

import jdk.incubator.vector.{VectorMask, VectorSpecies}

trait VectorOps[E] {

  def species: JSpecies[E]

  inline def lanes: Int = species.length()

  def broadcast(e: E): SimdVector[E]

  def zero: SimdVector[E]

  def reduceLanesAdd(v: SimdVector[E]): E

  def blend(a: SimdVector[E], b: SimdVector[E], mask: VectorMask[E]): SimdVector[E]

  transparent inline def fromVectorBatch[F[_]](fa: F[E], offset: Int)(using VectorBatch[F, E]): SimdVector[E] = {
    inline fa match {
      case arr: Array[E] => SimdVector[E](toJVector(arr, offset)(using species))
    }
  }

  transparent inline def toVectorBatch[F[_]](v: SimdVector[E], fa: F[E], offset: Int)(using VectorBatch[F, E]) = {
  inline fa match {
  case arr: Array[E] => 
    toArray(v, arr, offset)
    arr
  }
  }

  def fromArray(arr: Array[E], offset: Int): SimdVector[E]
  def toArray(v: SimdVector[E], arr: Array[E], offset: Int): Unit

}
