package mono.rlp

import akka.util.ByteString
import mono.Hash
import mono.rlp.RLPImplicits._

object RLPImplicitConversions {

  def fromEncodeable[T](value: RLPEncodeable)(implicit dec: RLPDecoder[T]): T = dec.decode(value)

  def fromRlpList[T](rlpList: RLPList)(implicit dec: RLPDecoder[T]): Seq[T] = rlpList.items.map(dec.decode)

  implicit def toEncodeable[T](value: T)(implicit enc: RLPEncoder[T]): RLPEncodeable = enc.encode(value)

  implicit def toRlpList[T](values: Seq[T])(implicit enc: RLPEncoder[T]): RLPList = RLPList(values.map(v => toEncodeable[T](v)): _*)

  implicit def hashFromEncodeable: (RLPEncodeable) => Hash = fromEncodeable[Hash]

  implicit def intFromEncodeable: (RLPEncodeable) => Int = fromEncodeable[Int]

  implicit def byteStringToEncodeable: (ByteString) => RLPEncodeable = toEncodeable[ByteString]

  implicit def byteStringFromEncodeable: (RLPEncodeable) => ByteString = fromEncodeable[ByteString]

  implicit def longFromEncodeable: (RLPEncodeable) => Long = fromEncodeable[Long]


  implicit def byteArrayFromEncodeable: (RLPEncodeable) => Array[Byte] = fromEncodeable[Array[Byte]]
}
