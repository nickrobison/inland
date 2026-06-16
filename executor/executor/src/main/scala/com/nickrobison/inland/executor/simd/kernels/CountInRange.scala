package com.nickrobison.inland.executor.simd.kernels

import com.nickrobison.inland.executor.VectorBatch
import com.nickrobison.inland.executor.simd.OrderOps

class CountInRange {

  def apply[E, F[_]](values: F[E], lower: E, upper: E)(using alg: OrderOps[E], tc: VectorBatch[F, E]): Int = {

    val lo = alg.broadcast(lower)
    val hi = alg.broadcast(upper)

    val size = tc.size(values)
    val lanes = alg.lanes

    var count = 0
    var i = 0
    while (i + lanes <= size) {
      val v = alg.fromVectorBatch(values, i)
      val ge = alg.gte(v, lo)
      val le = alg.lte(v, hi)

      var lane = 0
      while (lane < lanes) {
        if (ge.laneIsSet(lane) && le.laneIsSet(lane)) {
          count += 1
        }
        lane += 1
      }
      i += lanes
    }
    count
  }
}
