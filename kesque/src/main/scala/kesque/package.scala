import java.nio.ByteBuffer

package object kesque {

  /**
    * Special function to extract bytes from kafka's DefaultRecord key or value ByteBuffer
    * @see org.apache.kafka.common.utils.Utils.writeTo
    */
  private[kesque] def getBytes(buffer: ByteBuffer): Array[Byte] = {
    val length = buffer.remaining
    val value = Array.ofDim[Byte](length)
    if (buffer.hasArray) {
      System.arraycopy(buffer.array, buffer.position + buffer.arrayOffset, value, 0, length)
    } else {
      val pos = buffer.position
      var i = pos
      while (i < length + pos) {
        value(i) = buffer.get(i)
        i += 1
      }
    }
    value
  }
}
