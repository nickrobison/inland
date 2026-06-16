package com.nickrobison.inland.executor

import com.nickrobison.inland.executor.simd.{ArithOps, BitwiseOps, OrderOps, SimdVector}
import jdk.incubator.vector.VectorMask

object arith {
  extension [E](lhs: SimdVector[E])(using A: ArithOps[E]) {
    inline def +(rhs: SimdVector[E]): SimdVector[E] = A.plus(lhs, rhs)
    inline def -(rhs: SimdVector[E]): SimdVector[E] = A.minus(lhs, rhs)
    inline def *(rhs: SimdVector[E]): SimdVector[E] = A.mult(lhs, rhs)
    inline def /(rhs: SimdVector[E]): SimdVector[E] = A.div(lhs, rhs)
    inline def unary_- : SimdVector[E] = A.negate(lhs)
  }
}

object bitwise {
  extension [E](lhs: SimdVector[E])(using B: BitwiseOps[E]) {
    inline def &(rhs: SimdVector[E]): SimdVector[E] = B.and(lhs, rhs)
    inline def |(rhs: SimdVector[E]): SimdVector[E] = B.or(lhs, rhs)
    inline def ^(rhs: SimdVector[E]): SimdVector[E] = B.xor(lhs, rhs)
    inline def unary_~ : SimdVector[E] = B.not(lhs)
    inline def <<(rhs: Int): SimdVector[E] = B.shiftLeft(lhs, rhs)
    inline def >>(rhs: Int): SimdVector[E] = B.shiftRight(lhs, rhs)
  }
}

object ord {
  extension [E](lhs: SimdVector[E])(using O: OrderOps[E]) {
    inline def <(rhs: SimdVector[E]): VectorMask[E] = O.lt(lhs, rhs)
    inline def <=(rhs: SimdVector[E]): VectorMask[E] = O.lte(lhs, rhs)
    inline def >(rhs: SimdVector[E]): VectorMask[E] = O.gt(lhs, rhs)
    inline def >=(rhs: SimdVector[E]): VectorMask[E] = O.gte(lhs, rhs)
  }
}
