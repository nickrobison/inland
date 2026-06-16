package com.nickrobison.inland.executor.simd

trait FloatOps[E] extends ArithOps[E] {
  def sqrt(a: SimdVector[E]): SimdVector[E]
  def reciprocal(a: SimdVector[E]): SimdVector[E]
  def sin(a: SimdVector[E]): SimdVector[E]
  def cos(a: SimdVector[E]): SimdVector[E]
  def log(a: SimdVector[E]): SimdVector[E]
  def exp(a: SimdVector[E]): SimdVector[E]
}

object FloatOps {
  transparent inline def apply[E](using f: FloatOps[E]): f.type = f
}
