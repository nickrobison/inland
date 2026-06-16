package com.nickrobison.inland.executor.simd.kernels

import com.nickrobison.inland.executor.instances.array.{FloatInstances, LongInstances, arrayVector}
import com.nickrobison.inland.executor.simd.OrderOps
import org.scalatest.funsuite.AnyFunSuite

class CountInRangeSuite extends AnyFunSuite {

  // Float (float256 = 8 lanes)
  // ----------------------------------------------------------------

  test("Float all in range - size equals lanes") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f)
    assert(new CountInRange().apply(arr, 0f, 10f) == 8)
  }

  test("Float none in range - size equals lanes") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f)
    assert(new CountInRange().apply(arr, 10f, 20f) == 0)
  }

  test("Float partial range - size larger than lanes (exact multiple)") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(1f, 3f, 5f, 7f, 9f, 11f, 13f, 15f, 2f, 4f, 6f, 8f, 10f, 12f, 14f, 16f)
    assert(new CountInRange().apply(arr, 5f, 12f) == 8)
  }

  test("Float partial range - not a multiple of lanes") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(1f, 3f, 5f, 7f, 9f, 11f, 13f, 15f, 2f, 4f)
    assert(new CountInRange().apply(arr, 5f, 12f) == 4)
  }

  test("Float all in range - size smaller than lanes yields 0") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(1f, 2f, 3f)
    assert(new CountInRange().apply(arr, 0f, 10f) == 0)
  }

  test("Float none in range - size smaller than lanes") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(1f, 2f, 3f)
    assert(new CountInRange().apply(arr, 10f, 20f) == 0)
  }

  test("Float lower bound is inclusive") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(0f, 1f, 2f, 3f, 4f, 5f, 6f, 7f)
    assert(new CountInRange().apply(arr, 3f, 10f) == 5)
  }

  test("Float upper bound is inclusive") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(0f, 1f, 2f, 3f, 4f, 5f, 6f, 7f)
    assert(new CountInRange().apply(arr, 0f, 3f) == 4)
  }

  test("Float value one below lower bound is excluded") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(0f, 1f, 2f, 3f, 4f, 5f, 6f, 7f)
    assert(new CountInRange().apply(arr, 4f, 10f) == 4)
  }

  test("Float value one above upper bound is excluded") {
    given ops: OrderOps[Float] = FloatInstances.float256
    val arr = Array(0f, 1f, 2f, 3f, 4f, 5f, 6f, 7f)
    assert(new CountInRange().apply(arr, 0f, 2f) == 3)
  }

  // Long (long256 = 4 lanes)
  // ----------------------------------------------------------------

  test("Long all in range - size equals lanes") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(10L, 20L, 30L, 40L)
    assert(new CountInRange().apply(arr, 0L, 100L) == 4)
  }

  test("Long none in range - size equals lanes") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(10L, 20L, 30L, 40L)
    assert(new CountInRange().apply(arr, 50L, 100L) == 0)
  }

  test("Long partial range - size larger than lanes (exact multiple)") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L)
    assert(new CountInRange().apply(arr, 10L, 30L) == 5)
  }

  test("Long partial range - not a multiple of lanes") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L)
    assert(new CountInRange().apply(arr, 10L, 30L) == 5)
  }

  test("Long all in range - size smaller than lanes yields 0") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(100L, 200L)
    assert(new CountInRange().apply(arr, 0L, 300L) == 0)
  }

  test("Long none in range - size smaller than lanes") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(100L, 200L)
    assert(new CountInRange().apply(arr, 300L, 400L) == 0)
  }

  test("Long lower bound is inclusive") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(0L, 1L, 2L, 3L)
    assert(new CountInRange().apply(arr, 2L, 10L) == 2)
  }

  test("Long upper bound is inclusive") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(0L, 1L, 2L, 3L)
    assert(new CountInRange().apply(arr, 0L, 1L) == 2)
  }

  test("Long value one below lower bound is excluded") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(0L, 1L, 2L, 3L)
    assert(new CountInRange().apply(arr, 3L, 10L) == 1)
  }

  test("Long value one above upper bound is excluded") {
    given ops: OrderOps[Long] = LongInstances.long256
    val arr = Array(0L, 1L, 2L, 3L)
    assert(new CountInRange().apply(arr, 0L, 1L) == 2)
  }
}
