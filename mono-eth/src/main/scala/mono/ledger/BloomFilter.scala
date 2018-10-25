package mono.ledger

import akka.util.ByteString

object BloomFilter {

  private val BloomFilterByteSize = 256

  // alyways create new array instead of a val, since we cannot guarantee if it
  // will be changed outside (it's an array which is mutable)
  def emptyBloomFilterBytes = Array.ofDim[Byte](BloomFilterByteSize) // auto filled with 0
  def emptyBloomFilter = ByteString(emptyBloomFilterBytes)

}
