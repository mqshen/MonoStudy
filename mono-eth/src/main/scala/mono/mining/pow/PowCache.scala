package mono.mining.pow

import akka.util.ByteString
import mono.crypto

object PowCache {

  val JEpoch = 30000

  def seedForBlock(blockNumber: Long): ByteString = {
    var i = 0L
    val until = blockNumber / JEpoch
    var result = mono.hexDecode("00" * 32)
    while (i < until) {
      result = crypto.kec256(result)
      i += 1
    }
    ByteString(result)
  }

}
