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

  test("bytes helpers create correct values") {
    1L.mb shouldBe Bytes(1000000)
    1L.mib shouldBe Bytes(1048576)
    2L.gb shouldBe Bytes(2000000000)
    1L.kb shouldBe Bytes(1000)
    1L.kib shouldBe Bytes(1024)
  }

  test("bytes conversions return correct doubles") {
    val oneMB = 1L.mb
    oneMB.toMB shouldBe 1.0
    oneMB.toKB shouldBe 1000.0
    oneMB.toBytes shouldBe 1000000.0

    val oneMiB = 1L.mib
    oneMiB.toMiB shouldBe 1.0
    oneMiB.toKiB shouldBe 1024.0
    oneMiB.toBytes shouldBe 1048576.0
  }

  test("double bytes helpers create correct values") {
    1.5.mb.rawValue shouldBe 1500000L
    2.5.kib.rawValue shouldBe 2560L
  }

  test("rawValue returns underlying long") {
    42L.bytes.rawValue shouldBe 42L
  }

  // ── Runtime Refinement ──────────────────────────────────────────

  test("Bytes.option returns None on zero") {
    assert(Bytes.option(0L).isEmpty)
  }

  test("Bytes.option returns None on negative") {
    assert(Bytes.option(-1L).isEmpty)
  }

  test("Bytes.option returns Some on positive") {
    assert(Bytes.option(1L).isDefined)
  }

  test("Alignment.option returns None on zero") {
    assert(Alignment.option(0).isEmpty)
  }

  test("Alignment.option returns None on odd") {
    assert(Alignment.option(1).isEmpty)
  }

  test("Alignment.option returns Some on positive even") {
    assert(Alignment.option(2).isDefined)
  }

  test("Offset.option returns None on zero") {
    assert(Offset.option(0L).isEmpty)
  }

  test("Count.option returns None on zero") {
    assert(Count.option(0).isEmpty)
  }

  test("checkedBytes throws on zero") {
    assertThrows[IllegalArgumentException] { checkedBytes(0L) }
  }

  test("checkedBytes throws on negative") {
    assertThrows[IllegalArgumentException] { checkedBytes(-1L) }
  }

  test("Long extension .kb throws on zero") {
    assertThrows[IllegalArgumentException] { 0L.kb }
  }

  test("Double extension .mb throws on negative after truncation") {
    assertThrows[IllegalArgumentException] { (-0.5).mb }
  }
}
