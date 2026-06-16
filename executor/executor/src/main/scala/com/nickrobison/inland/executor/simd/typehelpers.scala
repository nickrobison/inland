package com.nickrobison.inland.executor.simd

import jdk.incubator.vector.{DoubleVector, FloatVector, IntVector, LongVector, ShortVector, VectorSpecies}

type JSpecies[T] = T match {
  case Int => VectorSpecies[java.lang.Integer]
  case Long => VectorSpecies[java.lang.Long]
  case Short => VectorSpecies[java.lang.Short]
  case Double => VectorSpecies[java.lang.Double]
  case Float => VectorSpecies[java.lang.Float]
}

type JVector[T] = T match {
  case Int => IntVector
  case Long => LongVector
  case Short => ShortVector
  case Double => DoubleVector
  case Float => FloatVector
}

transparent inline def toJVector[A](arry: Array[A], offset: Int)(using
    species: JSpecies[A]): JVector[A] = {
  inline arry match {
    case x: Array[Int] => IntVector.fromArray(species, x, offset)
    case x: Array[Long] => LongVector.fromArray(species, x, offset)
    case x: Array[Short] => ShortVector.fromArray(species, x, offset)
    case x: Array[Double] => DoubleVector.fromArray(species, x, offset)
    case x: Array[Float] => FloatVector.fromArray(species, x, offset)
  }
}

transparent inline def fromJVector[A](v: JVector[A], arry: Array[A], offset: Int)(using
    species: JSpecies[A]): Unit = {
  inline arry match {
    case x: Array[Int] => v.asInstanceOf[IntVector].intoArray(x, offset)
    case x: Array[Long] => v.asInstanceOf[LongVector].intoArray(x, offset)
    case x: Array[Short] => v.asInstanceOf[ShortVector].intoArray(x, offset)
    case x: Array[Double] => v.asInstanceOf[DoubleVector].intoArray(x, offset)
    case x: Array[Float] => v.asInstanceOf[FloatVector].intoArray(x, offset)
  }
}
