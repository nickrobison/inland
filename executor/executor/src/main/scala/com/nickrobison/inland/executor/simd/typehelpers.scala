package com.nickrobison.inland.executor.simd

import jdk.incubator.vector.{DoubleVector, FloatVector, IntVector, VectorSpecies}

type JSpecies[T] = T match {
  case Int => VectorSpecies[java.lang.Integer]
  case Double => VectorSpecies[java.lang.Double]
  case Float => VectorSpecies[java.lang.Float]
}

type JVector[T] = T match {
  case Int => IntVector
  case Double => DoubleVector
  case Float => FloatVector
}

transparent inline def toJVector[A](arry: Array[A], offset: Int)(using
    species: JSpecies[A]): JVector[A] = {
  inline arry match {
    case x: Array[Int] => IntVector.fromArray(species, x, offset)
    case x: Array[Double] => DoubleVector.fromArray(species, x, offset)
    case x: Array[Float] => FloatVector.fromArray(species, x, offset)
  }
}

transparent inline def fromJVector[A](v: JVector[A], arry: Array[A], offset: Int)(using
    species: JSpecies[A]): Unit = {
  inline arry match {
    case x: Array[Int] => v.asInstanceOf[IntVector].intoArray(x, offset)
    case x: Array[Double] => v.asInstanceOf[DoubleVector].intoArray(x, offset)
    case x: Array[Float] => v.asInstanceOf[FloatVector].intoArray(x, offset)
  }
}
