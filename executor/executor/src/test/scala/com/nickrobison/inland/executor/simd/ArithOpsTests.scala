package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.instances.array.{DoubleInstances, arrayVector}
import com.nickrobison.inland.executor.simd.{ArithOps, SimdVector}
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

    "subtract lane-wise" in {
      store(alg.minus(alg.fromArray(y, 0), alg.fromArray(x, 0))) shouldEqual
        Array(4.0, 4.0, 4.0, 4.0)
    }

    "multiply lane-wise" in {
      store(alg.mult(alg.fromArray(x, 0), alg.fromArray(y, 0))) shouldEqual
        Array(5.0, 12.0, 21.0, 32.0)
    }

    "negate all lanes" in {
      store(alg.negate(alg.fromArray(x, 0))) shouldEqual Array(-1.0, -2.0, -3.0, -4.0)
    }

    "abs restores negative lanes" in {
      store(alg.abs(alg.fromArray(Array(-1.0, -2.0, 3.0, -4.0), 0))) shouldEqual
        Array(1.0, 2.0, 3.0, 4.0)
    }

    "broadcast fills all lanes" in {
      store(alg.broadcast(3.14)).foreach(_ shouldBe 3.14 +- 1e-15)
    }

    "zero gives a zero vector" in {
      store(alg.zero) shouldEqual Array(0.0, 0.0, 0.0, 0.0)
    }

    "reduceLanesAdd sums all lanes" ignore {
      alg.reduceLanesAdd(alg.fromArray(x, 0)) shouldBe 10.0 +- 1e-12
    }

    "fma: a*b+c" ignore {
      store(alg.fma(alg.fromArray(x, 0), alg.fromArray(y, 0), alg.broadcast(1.0))) shouldEqual
        Array(6.0, 13.0, 22.0, 33.0)
    }

//    "operator syntax `x` inlines correctly" in {
//      store(alg.fromArray(x, 0) + alg.fromArray(y, 0)) shouldEqual
//        Array(6.0, 8.0, 10.0, 12.0)
//    }
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
