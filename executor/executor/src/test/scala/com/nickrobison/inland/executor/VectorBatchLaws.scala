package com.nickrobison.inland.executor

import com.nickrobison.inland.executor.VectorBatch
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

opaque type VectorIdx = Int

object VectorIdx {
  def apply(i: Int): VectorIdx = i
}

extension (i: VectorIdx) {
  def value: Int = i
}

trait VectorBatchLaws[F[_], A] extends Laws {

  def vec: VectorBatch[F, A]
  implicit def arbFa: Arbitrary[F[A]]
  implicit def arbA: Arbitrary[A]
  implicit def numeric: Numeric[A]
  implicit def arbVIdx: Arbitrary[VectorIdx]

  def laws: RuleSet = new DefaultRuleSet(
    name = "VectorBatch",
    parent = None,
    "size non-negative" -> forAll(sizeNonNegative),
    "isEmpty matches size" -> forAll(emptyMatchesSize),
    "get after set" -> forAll(getAfterSet)
  )

  private def sizeNonNegative(fa: F[A]): Prop = {
    vec.size(fa) >= 0
  }

  private def emptyMatchesSize(fa: F[A]): Prop = {
    vec.isEmpty(fa) == (vec.size(fa) == 0)
  }

  def getAfterSet(fa: F[A], idx: VectorIdx, a: A)(using Numeric[A]): Prop = {
    if (vec.isEmpty(fa)) {
      Prop.passed
    } else {
      val i = math.abs(idx.value) % vec.size(fa)
      vec.set(fa, i, a)
      vec.get(fa, i) == a
    }
  }
}

object VectorBatchLaws {
  def apply[F[_], A](using arb1: Arbitrary[F[A]], v: VectorBatch[F, A], arb2: Arbitrary[VectorIdx], arb3: Arbitrary[A], numA1: Numeric[A]): VectorBatchLaws[F, A] = new VectorBatchLaws[F, A] {
    override def vec: VectorBatch[F, A] = v

    override implicit def arbFa: Arbitrary[F[A]] = arb1

    override implicit def arbA: Arbitrary[A] = arb3

    override implicit def numeric: Numeric[A] = numA1

    override implicit def arbVIdx: Arbitrary[VectorIdx] = arb2
  }
}
