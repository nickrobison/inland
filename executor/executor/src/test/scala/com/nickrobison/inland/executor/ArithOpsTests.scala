package com.nickrobison.inland.executor

import com.nickrobison.inland.executor.instances.array.{DoubleInstances, arrayVector}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArithOpsTests extends AnyWordSpec with Matchers {

  given alg: ArithOps[Double] = DoubleInstances.double256

  val x: Array[Double] = Array(1.0, 2.0, 3.0, 4.0)
  val y = Array(5.0, 6.0, 7.0, 8.0)

  "ArithOps[Double] SPECIES_256" should {

    "round-trip via fromArray/toArray" in {
      store(alg.fromArray(x, 0)) shouldEqual x
    }

    "round-trip via VectorBatch" in {
      roundtripViaVectorBatch(y) shouldEqual y
    }

    "add lane-wise" in {
      store(alg.plus(alg.fromArray(x, 0), alg.fromArray(y, 0))) shouldEqual Array(6.0, 8.0, 10.0, 12.0)
    }
  }

  private inline def store(v: SimdVector[Double]): Array[Double] = {
    val out = new Array[Double](alg.lanes)
    alg.toArray(v, out, 0)
    out
  }

  private inline def roundtripViaVectorBatch(input: Array[Double]): Array[Double] = {
    val out = new Array[Double](alg.lanes)
    alg.toVectorBatch(alg.fromVectorBatch(input, 0), out, 0)
    out
  }

}
