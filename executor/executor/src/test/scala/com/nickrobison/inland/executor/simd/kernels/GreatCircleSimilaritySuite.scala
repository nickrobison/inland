package com.nickrobison.inland.executor.simd.kernels

import com.nickrobison.inland.executor.instances.array.{FloatInstances, arrayVector}
import com.nickrobison.inland.executor.simd.{ArithOps, FloatOps}
import org.scalatest.funsuite.AnyFunSuite

class GreatCircleSimilaritySuite extends AnyFunSuite {

  def scalarSimilarity(lat1: Float, lon1: Float, lat2: Float, lon2: Float): Float = {
    val slat1 = math.sin(lat1.toDouble).toFloat
    val slat2 = math.sin(lat2.toDouble).toFloat
    val clon1 = math.cos(lon1.toDouble).toFloat
    val clon2 = math.cos(lon2.toDouble).toFloat
    val cosD = math.cos((lon2 - lon1).toDouble).toFloat
    slat1 * slat2 + clon1 * clon2 * cosD
  }

  test("identical points at origin") {
    given alg: (ArithOps[Float] & FloatOps[Float]) = FloatInstances.float256
    val lat1 = Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f)
    val lon1 = Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f)
    val lat2 = Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f)
    val lon2 = Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f)
    val result = GreatCircleSimilarity(lat1, lon1, lat2, lon2)
    assert(result == 8f)
  }

  test("single pair") {
    given alg: (ArithOps[Float] & FloatOps[Float]) = FloatInstances.float256
    val lat1 = Array.fill(8)(0.5f)
    val lon1 = Array.fill(8)(0.3f)
    val lat2 = Array.fill(8)(0.7f)
    val lon2 = Array.fill(8)(0.2f)
    val result = GreatCircleSimilarity(lat1, lon1, lat2, lon2)
    val expected = 8f * scalarSimilarity(0.5f, 0.3f, 0.7f, 0.2f)
    assert(result == expected)
  }

  test("multiple pairs") {
    given alg: (ArithOps[Float] & FloatOps[Float]) = FloatInstances.float256
    val n = 8
    val lat1 = (0 until n).map(_.toFloat * 0.1f).toArray
    val lon1 = (0 until n).map(_.toFloat * 0.05f).toArray
    val lat2 = (0 until n).map(_.toFloat * 0.12f).toArray
    val lon2 = (0 until n).map(_.toFloat * 0.07f).toArray
    val result = GreatCircleSimilarity(lat1, lon1, lat2, lon2)
    val expected = (0 until n).map(i => scalarSimilarity(lat1(i), lon1(i), lat2(i), lon2(i))).sum
    assert(result == expected)
  }

  test("array shorter than lanes yields 0") {
    given alg: (ArithOps[Float] & FloatOps[Float]) = FloatInstances.float256
    val result = GreatCircleSimilarity(Array(1f), Array(2f), Array(3f), Array(4f))
    assert(result == 0f)
  }
}
