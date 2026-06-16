package com.nickrobison.inland.executor.simd.kernels

import com.nickrobison.inland.executor.VectorBatch
import com.nickrobison.inland.executor.simd.{ArithOps, FloatOps, SimdVector}

object GreatCircleSimilarity {

  def apply[E, F[_]](lat1: F[E], lon1: F[E], lat2: F[E], lon2: F[E])(using
      alg: ArithOps[E] & FloatOps[E],
      tc: VectorBatch[F, E]): E = {

    val size = tc.size(lat1)
    val lanes = alg.lanes

    var acc = alg.zero
    var i = 0

    while (i + lanes <= size) {
      val p1 = alg.fromVectorBatch(lat1, i)
      val l1 = alg.fromVectorBatch(lon1, i)
      val p2 = alg.fromVectorBatch(lat2, i)
      val l2 = alg.fromVectorBatch(lon2, i)

      val sin1 = alg.sin(p1)
      val sin2 = alg.sin(p2)

      val cos1 = alg.cos(l1)
      val cos2 = alg.cos(l2)

      val dLon = alg.minus(l2, l1)
      val cosD = alg.cos(dLon)

      val dd = dot(sin1, cos1, sin2, cos2, cosD)
      acc = alg.plus(acc, dd)

      i += lanes
    }

    alg.reduceLanesAdd(acc)
  }

  private def dot[E](
      sin1: SimdVector[E],
      cos1: SimdVector[E],
      sin2: SimdVector[E],
      cos2: SimdVector[E],
      cosD: SimdVector[E])(using alg: ArithOps[E] & FloatOps[E]): SimdVector[E] = {
    alg.fma(
      alg.mult(cos1, cos2),
      cosD,
      alg.mult(sin1, sin2)
    )

  }

}
