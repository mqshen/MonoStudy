package mono

import akka.util.ByteString

package object rlp {

  val ZeroByteRLP = RLP.byteToByteArray(0: Byte) // return Array()

  final case class RLPException(message: String) extends RuntimeException(message)

  sealed trait RLPEncodeable
  final case class RLPList(items: RLPEncodeable*) extends RLPEncodeable
  final case class RLPValue(bytes: Array[Byte]) extends RLPEncodeable {
    override def toString: String = s"RLPValue(${mono.toHexString(bytes)})"
  }



  trait RLPDecoder[T] {
    def decode(rlp: RLPEncodeable): T
  }
  // --- utilities for UInt256

  trait RLPSerializable {
    def toRLPEncodable: RLPEncodeable
    final def toBytes: Array[Byte] = encode(this.toRLPEncodable)
  }

  def toRLPEncodable(value: UInt256): RLPEncodeable =
    RLPValue(if (value.isZero) ZeroByteRLP else value.nonZeroLeadingBytes)

  trait RLPEncoder[T] {
    def encode(obj: T): RLPEncodeable
  }

  def encode[T](input: T)(implicit enc: RLPEncoder[T]): Array[Byte] = RLP.encode(enc.encode(input))
  def encode(input: RLPEncodeable): Array[Byte] = RLP.encode(input)

  def decode[T](data: Array[Byte])(implicit dec: RLPDecoder[T]): T = dec.decode(RLP.rawDecode(data))
  def decode[T](data: RLPEncodeable)(implicit dec: RLPDecoder[T]): T = dec.decode(data)

  def rawDecode(input: Array[Byte]): RLPEncodeable = RLP.rawDecode(input)

  def toUInt256(bytes: ByteString): UInt256 = toUInt256(bytes.toArray)
  def toUInt256(bytes: Array[Byte]): UInt256 = toUInt256(rawDecode(bytes))
  def toUInt256(rLPEncodeable: RLPEncodeable): UInt256 = {
    rLPEncodeable match {
      case RLPValue(bytes) => if (bytes.length == 0) UInt256.Zero else UInt256(bytes)
      case _               => throw RLPException("src is not an RLPValue")
    }
  }
}
