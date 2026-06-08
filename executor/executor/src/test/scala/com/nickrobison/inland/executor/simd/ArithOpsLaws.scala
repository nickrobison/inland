package com.nickrobison.inland.executor.simd

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

import scala.reflect.ClassTag

trait ArithOpsLaws[E: ClassTag] extends Laws {

  given ops: ArithOps[E] = scala.compiletime.deferred

  given eq: Eq[Array[E]] = scala.compiletime.deferred

  def laws(using arb: Arbitrary[Array[E]]): RuleSet = new DefaultRuleSet(
    name = "ArithOps",
    parent = None,
    "round-trip via fromArray/toArray" -> forAll(arrayRoundTrip)
  )

  private def arrayRoundTrip(v: Array[E]) = {
    eq.eqv(store(ops.fromArray(v, 0)), v)
  }

  private def store(v: SimdVector[E]): Array[E] = {
    val out = new Array[E](ops.lanes)
    ops.toArray(v, out, 0)
    out
  }

}

object ArithOpsLaws {
  def apply[E: ClassTag](using arithOps: ArithOps[E], eq: Eq[Array[E]]): ArithOpsLaws[E] = new ArithOpsLaws[E] {}
}
