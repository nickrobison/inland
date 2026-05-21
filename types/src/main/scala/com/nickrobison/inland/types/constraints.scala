package com.nickrobison.inland.types

import io.github.iltotore.iron.Constraint

final class Mod2

given Constraint[Int, Mod2] with {

  override inline def test(inline value: Int): Boolean = value % 2 == 0

  override inline def message: String = "Must be a multiple of 2"
}

given Constraint[Long, Mod2] with {
  override inline def test(inline value: Long): Boolean = value % 2 == 0

  override inline def message: String = "Must be a multiple of 2"
}
