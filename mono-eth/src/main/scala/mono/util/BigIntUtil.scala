package mono.util

import java.math.BigInteger

object BigIntUtil {
  def toUnsignedByteArray(n: BigInteger): Array[Byte] = {
    val bytes = n.toByteArray
    if (bytes(0) == 0) {
      val tail = Array.ofDim[Byte](bytes.length - 1)
      System.arraycopy(bytes, 1, tail, 0, tail.length)
      tail
    } else {
      bytes
    }
  }

  /**
   * @param value - not null
   * @return true - if the param is zero
   */
  def isZero(value: BigInteger): Boolean = {
    value.compareTo(BigInteger.ZERO) == 0
  }

  /**
   * @param valueA - not null
   * @param valueB - not null
   * @return true - if the valueA is equal to valueB is zero
   */
  def isEqual(valueA: BigInteger, valueB: BigInteger): Boolean = {
    valueA.compareTo(valueB) == 0
  }

  /**
   * @param valueA - not null
   * @param valueB - not null
   * @return true - if the valueA is not equal to valueB is zero
   */
  def isNotEqual(valueA: BigInteger, valueB: BigInteger): Boolean = {
    !isEqual(valueA, valueB)
  }

  /**
   * @param valueA - not null
   * @param valueB - not null
   * @return true - if the valueA is less than valueB is zero
   */
  def isLessThan(valueA: BigInteger, valueB: BigInteger): Boolean = {
    valueA.compareTo(valueB) < 0
  }

  /**
   * @param valueA - not null
   * @param valueB - not null
   * @return true - if the valueA is more than valueB is zero
   */
  def isMoreThan(valueA: BigInteger, valueB: BigInteger): Boolean = {
    valueA.compareTo(valueB) > 0
  }

  /**
   * @param valueA - not null
   * @param valueB - not null
   * @return sum - valueA + valueB
   */
  def sum(valueA: BigInteger, valueB: BigInteger): BigInteger = {
    valueA.add(valueB)
  }

  /**
   * @param data = not null
   * @return new positive BigInteger
   */
  def toBI(data: Array[Byte]): BigInteger = {
    new BigInteger(1, data)
  }

  /**
   * @param data = not null
   * @return new positive BigInteger
   */
  def toBI(data: Long): BigInteger = {
    BigInteger.valueOf(data)
  }

  def isPositive(value: BigInteger): Boolean = {
    value.signum() > 0
  }

  def isCovers(covers: BigInteger, value: BigInteger): Boolean = {
    !isNotCovers(covers, value)
  }

  def isNotCovers(covers: BigInteger, value: BigInteger): Boolean = {
    covers.compareTo(value) < 0
  }

  def exitLong(value: BigInteger): Boolean = {
    (value.compareTo(new BigInteger(Long.MaxValue + ""))) > -1
  }

  def isIn20PercentRange(first: BigInteger, second: BigInteger): Boolean = {
    val five = BigInteger.valueOf(5)
    val limit = first.add(first.divide(five))
    !isMoreThan(second, limit)
  }

  def max(first: BigInteger, second: BigInteger): BigInteger = {
    if (first.compareTo(second) < 0) second else first
  }

  /**
   * Returns a result of safe addition of two {@code int} values
   * {@code Integer.MAX_VALUE} is returned if overflow occurs
   */
  def addSafely(a: Int, b: Int): Int = {
    val res = a.toLong + b.toLong
    if (res > Int.MaxValue) Int.MaxValue else res.toInt
  }
}
