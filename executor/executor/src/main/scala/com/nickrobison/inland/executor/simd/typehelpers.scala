package com.nickrobison.inland.executor.simd

import jdk.incubator.vector.{DoubleVector, IntVector, VectorSpecies}

type JSpecies[T] = T match {
  case Int => VectorSpecies[java.lang.Integer]
  case Double => VectorSpecies[java.lang.Double]
}

type JVector[T] = T match {
  case Int => jdk.incubator.vector.Vector[java.lang.Integer]
  case Double => jdk.incubator.vector.Vector[java.lang.Double]
}

transparent inline def toJVector[A](arry: Array[A], offset: Int)(using
    species: JSpecies[A]): JVector[A] = {
  inline arry match {
    case x: Array[Int] => IntVector.fromArray(species, x, offset)
    case x: Array[Double] => DoubleVector.fromArray(species, x, offset)
  }
}

transparent inline def fromJVector[A](v: JVector[A], arry: Array[A], offset: Int)(using
    species: JSpecies[A]): Unit = {
  inline arry match {
    case x: Array[Int] => v.asInstanceOf[IntVector].intoArray(x, offset)
    case x: Array[Double] => v.asInstanceOf[DoubleVector].intoArray(x, offset)
  }
}
