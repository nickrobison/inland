package com.nickrobison.inland.executor

import cats.Eq
import com.nickrobison.inland.executor.VectorBatch
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/**
 * Verifies that [[ArithOps]] hold for the given [[VectorBatch]] instance
 */
trait AirthOpsLaws[F[_], E] extends Laws {

  type Vec = F[E]

  given vb: VectorBatch[F, E] = scala.compiletime.deferred
  given ops: ArithOps[E] = scala.compiletime.deferred

  def laws(using arb: Arbitrary[Vec], eq: Eq[SimdVector[E]]): RuleSet = new DefaultRuleSet(
    name = "VectorBatch",
    parent = None,
    "add_commutative" -> forAll(add_commutative)
  )

  private def add_commutative(a: Vec, b: F[E])(using eq: Eq[SimdVector[E]]) = {
    eq.eqv(
      ops.plus(ops.fromVectorBatch(a, 0), ops.fromVectorBatch(b, 0)),
      ops.plus(ops.fromVectorBatch(b, 0), ops.fromVectorBatch(a, 0)))

  }

}

object ArithOpsLaws {
  def apply[F[_], E](using VectorBatch[F, E], ArithOps[E]): AirthOpsLaws[F, E] =
    new AirthOpsLaws[F, E] {}
}
