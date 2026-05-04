package com.nickrobison.inland.executor

trait VectorBatch[F[_], A] {
  def get(fa: F[A], i: Int): A
  def set(fa: F[A], i: Int, a: A): Unit
  def size(fa: F[A]): Int
  def isEmpty(fa: F[A]): Boolean
}

object VectorBatch {
  def apply[F[_], A](using v: VectorBatch[F, A]): VectorBatch[F, A] = v
}
