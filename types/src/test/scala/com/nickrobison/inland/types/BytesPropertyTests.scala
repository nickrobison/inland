package com.nickrobison.inland.types

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class BytesPropertyTests extends AnyFunSuite with Matchers with Checkers {

  // Generate positive Long values that won't overflow when multiplied
  // Max safe value for all multipliers is Long.MaxValue / (1024^4) ~ 1TB
  private val maxSafe = Long.MaxValue / (1024L * 1024L * 1024L * 1024L)

  given Arbitrary[Long] = Arbitrary(Gen.choose(1L, maxSafe))

  // Roundtrip properties: Long -> Bytes -> Double -> should match original (approximately for large values)
  // For exact roundtrips we need small integer values
  given Arbitrary[Int] = Arbitrary(Gen.choose(1, 1000000))

  test("bytes identity roundtrip") {
    check(forAll { (n: Long) =>
      n.bytes.rawValue == n
    })
  }

  test("kb roundtrip for integer values") {
    check(forAll { (n: Int) =>
      n.kb.toKB == n.toDouble
    })
  }

  test("mb roundtrip for integer values") {
    check(forAll { (n: Int) =>
      n.mb.toMB == n.toDouble
    })
  }

  test("gb roundtrip for integer values") {
    check(forAll { (n: Int) =>
      n.gb.toGB == n.toDouble
    })
  }

  test("tb roundtrip for small integer values") {
    check(forAll(Gen.choose(1, 10000)) { (n: Int) =>
      n.tb.toTB == n.toDouble
    })
  }

  test("kib roundtrip for integer values") {
    check(forAll { (n: Int) =>
      n.kib.toKiB == n.toDouble
    })
  }

  test("mib roundtrip for integer values") {
    check(forAll { (n: Int) =>
      n.mib.toMiB == n.toDouble
    })
  }

  test("gib roundtrip for small integer values") {
    check(forAll(Gen.choose(1, 10000)) { (n: Int) =>
      n.gib.toGiB == n.toDouble
    })
  }

  test("tib roundtrip for small integer values") {
    check(forAll(Gen.choose(1, 100)) { (n: Int) =>
      n.tib.toTiB == n.toDouble
    })
  }

  test("decimal units are consistent") {
    check(forAll { (n: Int) =>
      n.mb.toKB == n.toDouble * 1000.0
    })
  }

  test("binary units are consistent") {
    check(forAll { (n: Int) =>
      n.mib.toKiB == n.toDouble * 1024.0
    })
  }

  test("bytes to all units are consistent for decimal") {
    check(forAll { (n: Int) =>
      val b = n.kb
      b.toKB == b.toBytes / 1000.0
    })
  }

  test("bytes to all units are consistent for binary") {
    check(forAll { (n: Int) =>
      val b = n.kib
      b.toKiB == b.toBytes / 1024.0
    })
  }

  test("rawValue equals underlying bytes") {
    check(forAll { (n: Long) =>
      val b = n.bytes
      b.rawValue == b.value
    })
  }

  test("decimal and binary are different for same number") {
    check(forAll { (n: Int) =>
      n.mb != n.mib || n == 0
    })
  }

  test("double kb conversion matches long kb") {
    check(forAll { (n: Int) =>
      n.toDouble.kb.rawValue == n.kb.rawValue
    })
  }

  test("double mb conversion matches long mb") {
    check(forAll { (n: Int) =>
      n.toDouble.mb.rawValue == n.mb.rawValue
    })
  }

  test("double gb conversion matches long gb") {
    check(forAll { (n: Int) =>
      n.toDouble.gb.rawValue == n.gb.rawValue
    })
  }

  test("double kib conversion matches long kib") {
    check(forAll { (n: Int) =>
      n.toDouble.kib.rawValue == n.kib.rawValue
    })
  }

  test("double mib conversion matches long mib") {
    check(forAll { (n: Int) =>
      n.toDouble.mib.rawValue == n.mib.rawValue
    })
  }

  test("double gib conversion matches long gib") {
    check(forAll { (n: Int) =>
      n.toDouble.gib.rawValue == n.gib.rawValue
    })
  }

  test("1 mb equals 1000000 bytes") {
    assert(1L.mb.rawValue == 1000000L)
  }

  test("1 mib equals 1048576 bytes") {
    assert(1L.mib.rawValue == 1048576L)
  }

  test("1 gb equals 1000000000 bytes") {
    assert(1L.gb.rawValue == 1000000000L)
  }

  test("1 gib equals 1073741824 bytes") {
    assert(1L.gib.rawValue == 1073741824L)
  }

  test("1 tb equals 1000000000000 bytes") {
    assert(1L.tb.rawValue == 1000000000000L)
  }

  test("1 tib equals 1099511627776 bytes") {
    assert(1L.tib.rawValue == 1099511627776L)
  }

  test("cross unit decimal consistency") {
    check(forAll { (n: Int) =>
      val bytes = n.mb
      bytes.toKB == n.toDouble * 1000.0 &&
      bytes.toGB == n.toDouble / 1000.0
    })
  }

  test("cross unit binary consistency") {
    check(forAll { (n: Int) =>
      val bytes = n.mib
      bytes.toKiB == n.toDouble * 1024.0 &&
      bytes.toGiB == n.toDouble / 1024.0
    })
  }
}