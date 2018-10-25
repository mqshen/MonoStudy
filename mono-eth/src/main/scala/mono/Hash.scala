package mono

import java.util.Arrays

import akka.util.ByteString

object Hash {
  val empty = Hash(Array.emptyByteArray)
  def apply(): Hash = empty

  def intHash(bytes: Array[Byte]): Int = {
    val n = math.min(bytes.length, 4)
    var h = 0
    var i = 0
    while (i < n) {
      h <<= 8
      h |= (bytes(i) & 0xFF)
      i += 1
    }
    h
  }

  trait I {
    val bytes: Array[Byte]

    final def value = new java.math.BigInteger(1, bytes)

    final def length = bytes.length
    final def isEmpty = bytes.length == 0
    final def nonEmpty = bytes.length != 0

    final def toByteString = ByteString(bytes)
    final def hexString: String = mono.toHexString(bytes)

    final override def hashCode: Int = intHash(bytes)

    final override def toString: String = hexString
  }

}

final case class Hash(bytes: Array[Byte]) extends Hash.I {
  override def equals(any: Any) = any match {
    case that: Hash => Arrays.equals(this.bytes, that.bytes)
    case _          => false
  }
}