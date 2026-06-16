package com.nickrobison.inland.executor.simd

import com.nickrobison.inland.executor.VectorBatch
import com.nickrobison.inland.executor.instances.array.{arrayVector, scalaVectorInstance, DoubleInstances, FloatInstances, IntInstances}
import com.nickrobison.inland.executor.simd.ArithOpsLaws.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class ContainerAlgebraTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll("Array[Int]ContainerTests", ContainerLaws[Array, Int].laws(
    using IntInstances.intPreferred, summon[VectorBatch[Array, Int]]))
  checkAll("Array[Double]ContainerTests", ContainerLaws[Array, Double].laws(
    using DoubleInstances.double256, summon[VectorBatch[Array, Double]]))
  checkAll("Array[Float]ContainerTests", ContainerLaws[Array, Float].laws(
    using FloatInstances.float256, summon[VectorBatch[Array, Float]]))

  checkAll("scala.Vector[Int]ContainerTests", ContainerLaws[scala.Vector, Int].batchRead(
    using IntInstances.intPreferred, summon[VectorBatch[scala.Vector, Int]]))
  checkAll("scala.Vector[Double]ContainerTests", ContainerLaws[scala.Vector, Double].batchRead(
    using DoubleInstances.double256, summon[VectorBatch[scala.Vector, Double]]))
  checkAll("scala.Vector[Float]ContainerTests", ContainerLaws[scala.Vector, Float].batchRead(
    using FloatInstances.float256, summon[VectorBatch[scala.Vector, Float]]))
}
