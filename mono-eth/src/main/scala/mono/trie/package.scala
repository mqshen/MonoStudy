package mono

package object trie {

  trait ByteArrayEncoder[T] {
    def toBytes(input: T): Array[Byte]
  }

  trait ByteArrayDecoder[T] {
    def fromBytes(bytes: Array[Byte]): T
  }

  trait ByteArraySerializable[T] extends ByteArrayEncoder[T] with ByteArrayDecoder[T]

  def toHash(bytes: Array[Byte]): Array[Byte] = crypto.kec256(bytes)

  import mono.rlp.RLPImplicits._
  val EmptyTrieHash = toHash(rlp.encode(Array.ofDim[Byte](0)))

  object byteArraySerializable extends ByteArraySerializable[Array[Byte]] {
    override def toBytes(input: Array[Byte]): Array[Byte] = input
    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
  }
}
