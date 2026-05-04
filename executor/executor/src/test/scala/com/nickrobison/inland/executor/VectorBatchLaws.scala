package com.nickrobison.inland.executor

import com.nickrobison.inland.executor.VectorBatch
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import scala.reflect.ClassTag

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

  def laws(using ClassTag[A]): RuleSet = new DefaultRuleSet(
    name = "VectorBatch",
    parent = None,
    "size non-negative" -> forAll(sizeNonNegative),
    "isEmpty matches size" -> forAll(emptyMatchesSize),
    "toArray matches sizes" -> forAll(toArrayMatchesLength),
    "toArray consistent with get" -> forAll(toArrayIsTotal),
    "fold touches all elements" -> forAll(foldLeftAllElements),
    "fold preserves order" -> forAll(foldLeftPreservesOrder),
    "get after set" -> forAll(getAfterSet)
  )

  private def sizeNonNegative(fa: F[A]): Prop = {
    vec.size(fa) >= 0
  }

  private def emptyMatchesSize(fa: F[A]): Prop = {
    vec.isEmpty(fa) == (vec.size(fa) == 0)
  }

  private def toArrayMatchesLength(fa: F[A])(using ClassTag[A]): Prop = {
    val n = vec.size(fa)
    val arr = vec.toArray(fa)
    n == arr.length
  }

  private def getAfterSet(fa: F[A], idx: VectorIdx, a: A)(using Numeric[A]): Prop = {
    if (vec.isEmpty(fa)) {
      Prop.passed
    } else {
      val i = math.abs(idx.value) % vec.size(fa)
      vec.set(fa, i, a)
      vec.get(fa, i) == a
    }
  }

  private def toArrayIsTotal(fa: F[A])(using ClassTag[A]): Prop = {
    val arr = vec.toArray(fa)
    Prop.all(
      (0 until vec.size(fa)).map { i =>
        Prop(arr(i) == vec.get(fa, i))
      }*
    )
  }

  private def foldLeftAllElements(fa: F[A]): Prop = {
    val count = vec.foldLeft(fa)(0)((acc, _) => acc + 1)
    count == vec.size(fa)
  }

  def foldLeftPreservesOrder(fa: F[A])(using ClassTag[A]): Prop = {
    val fromFold = vec.foldLeft(fa)(List.empty[A])((acc, a) => acc :+ a)
    val fromGet = (0 until vec.size(fa)).map(vec.get(fa, _)).toList
    fromFold == fromGet
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
