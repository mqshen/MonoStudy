package mono.vm

import mono.UInt256_biginteger.UInt256

object Stack {

  /**
    * Stack max size as defined in the YP (9.1)
    */
  val DefaultMaxSize = 1024
  val ListOfZero = List(UInt256.Zero)

  def empty(maxSize: Int = DefaultMaxSize): Stack = new Stack(maxSize)

}

final class Stack private (val maxSize: Int) {

}
