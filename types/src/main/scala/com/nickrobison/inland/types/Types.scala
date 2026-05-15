package com.nickrobison.inland.types

import io.github.iltotore.iron.RefinedType
import io.github.iltotore.iron.constraint.all.{Greater, Positive}
import io.github.iltotore.iron.constraint.any.DescribedAs

import scala.annotation.targetName

type Alignment = Alignment.T
object Alignment extends RefinedType[Int, Positive & Mod2]

type Offset = Offset.T
object Offset extends RefinedType[Long, Positive]

type Bytes = Bytes.T
object Bytes extends RefinedType[Long, Positive]

extension (x: Bytes) {
  def * (y: Count): Bytes = {
    Bytes.applyUnsafe(x.value * y.value)
  }
}

type Count = Count.T
object Count extends RefinedType[Int, Positive]

extension (x: Count) {
  def * (y: Bytes): Bytes = {
    Bytes.applyUnsafe(x.value * y.value)
  }
}

type Aligned = Aligned.T
object Aligned extends RefinedType[Long, Positive & Mod2]

