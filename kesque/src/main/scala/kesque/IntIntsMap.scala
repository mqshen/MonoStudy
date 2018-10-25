package kesque

/**
  * It's acutally an Int to Ints map
  * We introduce one extra pairs of fields - for key=0, which is used as 'used' flag
  * Memoey usage:
  *   Int -> Int[]
  * Key (Int) in bytes: 4
  * Value (Int[]) in bytes: (16(reference) + 4(length) + 4(align)) + 4 * N = 24 + 4 * N
  * KV in Bytes: 28 + 4 * N
  *
  * There are about 99.6% keys have only 1 value, i.e. 28 + 4 = 32 bytes.
  * Thus 100,000,000 kvs in bytes: 100,000,000 * 32 / 1024 / 1024 / 1024 = 3G
  */
final class IntIntsMap(initSize: Int, nValues: Int, fillFactor: Float = 0.75f) {
  /** Current map size */
  private var m_size: Int = _

  def size = m_size

}
