package com.nickrobison.inland.types

/**
 * Unit conversion constants for byte multipliers
 * Decimal: powers of 1000 (kB, MB, GB, TB)
 * Binary: powers of 1024 (KiB, MiB, GiB, TiB)
 */
private inline val DECIMAL_KB = 1000L
private inline val DECIMAL_MB = DECIMAL_KB * 1000L
private inline val DECIMAL_GB = DECIMAL_MB * 1000L
private inline val DECIMAL_TB = DECIMAL_GB * 1000L

private inline val BINARY_KIB = 1024L
private inline val BINARY_MIB = BINARY_KIB * 1024L
private inline val BINARY_GIB = BINARY_MIB * 1024L
private inline val BINARY_TIB = BINARY_GIB * 1024L

/**
 * Extensions on Long for creating Bytes instances with various units
 */
extension (value: Long)
  /** Create Bytes from a raw byte count (identity) */
  def bytes: Bytes = Bytes.applyUnsafe(value)

  /** Create Bytes from kilobytes (decimal, 1000) */
  def kb: Bytes = Bytes.applyUnsafe(value * DECIMAL_KB)

  /** Create Bytes from megabytes (decimal, 1000²) */
  def mb: Bytes = Bytes.applyUnsafe(value * DECIMAL_MB)

  /** Create Bytes from gigabytes (decimal, 1000³) */
  def gb: Bytes = Bytes.applyUnsafe(value * DECIMAL_GB)

  /** Create Bytes from terabytes (decimal, 1000⁴) */
  def tb: Bytes = Bytes.applyUnsafe(value * DECIMAL_TB)

  /** Create Bytes from kibibytes (binary, 1024) */
  def kib: Bytes = Bytes.applyUnsafe(value * BINARY_KIB)

  /** Create Bytes from mebibytes (binary, 1024²) */
  def mib: Bytes = Bytes.applyUnsafe(value * BINARY_MIB)

  /** Create Bytes from gibibytes (binary, 1024³) */
  def gib: Bytes = Bytes.applyUnsafe(value * BINARY_GIB)

  /** Create Bytes from tebibytes (binary, 1024⁴) */
  def tib: Bytes = Bytes.applyUnsafe(value * BINARY_TIB)

/**
 * Extensions on Double for creating Bytes instances with various units
 * Values are truncated to Long precision
 */
extension (value: Double)
  /** Create Bytes from a raw double byte count (truncated) */
  def bytes: Bytes = Bytes.applyUnsafe(value.toLong)

  /** Create Bytes from fractional kilobytes (decimal, truncated) */
  def kb: Bytes = Bytes.applyUnsafe((value * DECIMAL_KB).toLong)

  /** Create Bytes from fractional megabytes (decimal, truncated) */
  def mb: Bytes = Bytes.applyUnsafe((value * DECIMAL_MB).toLong)

  /** Create Bytes from fractional gigabytes (decimal, truncated) */
  def gb: Bytes = Bytes.applyUnsafe((value * DECIMAL_GB).toLong)

  /** Create Bytes from fractional terabytes (decimal, truncated) */
  def tb: Bytes = Bytes.applyUnsafe((value * DECIMAL_TB).toLong)

  /** Create Bytes from fractional kibibytes (binary, truncated) */
  def kib: Bytes = Bytes.applyUnsafe((value * BINARY_KIB).toLong)

  /** Create Bytes from fractional mebibytes (binary, truncated) */
  def mib: Bytes = Bytes.applyUnsafe((value * BINARY_MIB).toLong)

  /** Create Bytes from fractional gibibytes (binary, truncated) */
  def gib: Bytes = Bytes.applyUnsafe((value * BINARY_GIB).toLong)

  /** Create Bytes from fractional tebibytes (binary, truncated) */
  def tib: Bytes = Bytes.applyUnsafe((value * BINARY_TIB).toLong)

/**
 * Extensions on Bytes for converting back to various units as Double
 */
extension (bytes: Bytes)
  /** Convert to raw bytes (identity as Double) */
  def toBytes: Double = bytes.value.toDouble

  /** Convert to kilobytes (decimal) */
  def toKB: Double = bytes.value.toDouble / DECIMAL_KB

  /** Convert to megabytes (decimal) */
  def toMB: Double = bytes.value.toDouble / DECIMAL_MB

  /** Convert to gigabytes (decimal) */
  def toGB: Double = bytes.value.toDouble / DECIMAL_GB

  /** Convert to terabytes (decimal) */
  def toTB: Double = bytes.value.toDouble / DECIMAL_TB

  /** Convert to kibibytes (binary) */
  def toKiB: Double = bytes.value.toDouble / BINARY_KIB

  /** Convert to mebibytes (binary) */
  def toMiB: Double = bytes.value.toDouble / BINARY_MIB

  /** Convert to gibibytes (binary) */
  def toGiB: Double = bytes.value.toDouble / BINARY_GIB

  /** Convert to tebibytes (binary) */
  def toTiB: Double = bytes.value.toDouble / BINARY_TIB

  /** Access the raw Long value */
  def rawValue: Long = bytes.value