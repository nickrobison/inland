package com.nickrobison.inland.executor.simd

trait ArithOps[E] extends VectorOps[E] {

  def plus(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def minus(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def mult(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def div(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def negate(a: SimdVector[E]): SimdVector[E]
  def abs(a: SimdVector[E]): SimdVector[E]
  def fma(a: SimdVector[E], b: SimdVector[E], c: SimdVector[E]): SimdVector[E]
}
