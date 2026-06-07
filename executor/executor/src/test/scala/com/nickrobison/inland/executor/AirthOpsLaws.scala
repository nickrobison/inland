//package com.nickrobison.inland.executor
//
//import cats.Eq
//import org.scalacheck.Arbitrary
//import org.scalacheck.Prop.forAll
//import org.typelevel.discipline.Laws
//import com.nickrobison.inland.executor.VectorBatch
//
//trait AirthOpsLaws[F[_], A] extends Laws {
//
//  type Vec = VectorBatch[F, A]
//
//  given simdEq(using eqv: Eq[Vec], vo: VectorOps[A]): Eq[SimdVector[A]] with {
//    def eqv(x: SimdVector[A], y: SimdVector[A]): Boolean = {
//      Array.equals(x.underlying, y.underlying)
//    }
//  }
//
//  def laws(using ops: ArithOps[A], arb: Arbitrary[Vec], eq: Eq[Vec]): RuleSet = new DefaultRuleSet(
//    name = "VectorBatch",
//    parent = None,
//    "add_commutative" -> forAll(add_commutative)
//  )
//
//
//
//  private def add_commutative(a: SimdVector[A], b: SimdVector[A])(using ops: ArithOps[A], eq: Eq[SimdVector[A]]) = {
//    eq.eqv(ops.plus(a, b), ops.plus(b, a))
//  }
//
//}
//
//object ArithOpsLaws {
//  def apply[F[_], A]: AirthOpsLaws[F, A] = new AirthOpsLaws[F, A] {}
//}
