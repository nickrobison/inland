package com.nickrobison.inland.executor.simd

opaque type SimdVector[A] = JVector[A]

object SimdVector {
  transparent inline def apply[E](v: JVector[E]): SimdVector[E] = v

  extension [E](v: SimdVector[E]) {
    transparent inline def underlying: JVector[E] = v
  }
}