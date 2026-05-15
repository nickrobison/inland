package com.nickrobison.inland.allocator

object CommonErrors {
  def indexOutOfBounds(index: Int, max: Int): IndexOutOfBoundsException =
    new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max $max)")

  /** IndexOutOfBounds exception with an unknown max index. */
  def indexOutOfBounds(index: Int): IndexOutOfBoundsException =
    new IndexOutOfBoundsException(s"$index is out of bounds (min 0, max unknown)")

}
