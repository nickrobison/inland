package com.nickrobison.inland.executor.instances.array

import com.nickrobison.inland.executor.VectorBatch

inline given arrayVector[A]: VectorBatch[Array, A] with {
  def get(fa: Array[A], i: Int): A = fa(i)

  def set(fa: Array[A], i: Int, a: A): Unit = fa(i) = a

  def size(fa: Array[A]): Int = fa.length

  def isEmpty(fa: Array[A]): Boolean = size(fa) == 0

}
