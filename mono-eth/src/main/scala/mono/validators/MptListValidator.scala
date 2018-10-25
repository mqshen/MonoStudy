package mono.validators

import mono.rlp
import mono.rlp.RLPImplicits._
import mono.trie.ByteArraySerializable

object MptListValidator {

  lazy val intByteArraySerializable = new ByteArraySerializable[Int] {
    override def fromBytes(bytes: Array[Byte]): Int = rlp.decode[Int](bytes)
    override def toBytes(input: Int): Array[Byte] = rlp.encode(input)
  }

}
