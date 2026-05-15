package com.nickrobison.inland.types

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SimpleTypeTests extends AnyFunSuite with Matchers {

  test("Simple type tests") {
    Offset(1) shouldBe Offset(1)
  }

  test("bytes * count is communitive") {
    (Count(10) * Bytes(10)) shouldBe (Bytes(10) * Count(10))
  }

  test("Alignment should be mod 2") {
    assertDoesNotCompile(
      """
        | Alignment(1)
        |""".stripMargin)
    assertDoesNotCompile(
      """
        | Alignment(0)
        |""".stripMargin
    )
  }
  
  test("")

}
