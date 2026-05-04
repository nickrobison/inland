package com.nickrobison.inland.executor

import scala.reflect.ClassTag

trait VectorBatch[F[_], A] {
  def get(fa: F[A], i: Int): A
  def set(fa: F[A], i: Int, a: A): Unit
  def size(fa: F[A]): Int
  def isEmpty(fa: F[A]): Boolean

  def toArray(fa: F[A])(using ClassTag[A]): Array[A] = {
    val n = size(fa)
    val arr = new Array[A](n)
    var i = 0
    while (i < n) {
      arr(i) = get(fa, i)
      i += 1
    }
    arr
  }

  inline def foldLeft[B](fa: F[A])(z: B)(inline f: (B, A) => B): B = {
    val n = size(fa)
    var acc = z
    var i = 0
    while (i < n) {
      acc = f(acc, get(fa, i))
      i += 1
    }
    acc
  }
}

object VectorBatch {
  def apply[F[_], A](using v: VectorBatch[F, A]): VectorBatch[F, A] = v
}
