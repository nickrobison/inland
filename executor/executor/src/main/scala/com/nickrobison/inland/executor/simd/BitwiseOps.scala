package com.nickrobison.inland.executor.simd

trait BitwiseOps[E] extends ArithOps[E] with OrderOps[E] {

  def and(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def or(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def xor(a: SimdVector[E], b: SimdVector[E]): SimdVector[E]
  def not(a: SimdVector[E]): SimdVector[E]
  def shiftLeft(a: SimdVector[E], n: Int): SimdVector[E]
  def shiftRight(a: SimdVector[E], n: Int): SimdVector[E]
  def signedRightRigh(a: SimdVector[E], n: Int): SimdVector[E]

}
