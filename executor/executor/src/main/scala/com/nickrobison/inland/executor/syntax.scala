//package com.nickrobison.inland.executor
//
//import jdk.incubator.vector.VectorMask
//
//object arith {
//  extension[F[_], E](lhs: VectorBatch[F, E])(using A: ArithOps[E]) {
//    inline def +(rhs: A.Vec): A.Vec = A.plus(lhs, rhs)
//    // TODO: Rest
//    inline def unary_~ : A.Vec = A.negate(lhs)
//  }
//}
//
//object bitwise {
//  extension[F[_], E](lhs: VectorBatch[F, E])(using B: BitwiseOps[ E]) {
//    // TODO: Rest
//    inline def &(rhs: B.Vec): B.Vec = B.and(lhs, rhs)
//  }
//}
//
//object ord {
//  extension[F[_], E](lhs: VectorBatch[F, E])(using O: OrderOps[E]) {
//    // TODO: Rest
//    inline def <(rhs: O.Vec): VectorMask[E] = O.lt(lhs, rhs)
//    inline def <=(rhs: O.Vec): VectorMask[E] = O.lte(lhs, rhs)
//  }
//}