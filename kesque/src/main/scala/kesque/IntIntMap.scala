package kesque

/**
  * How about if the int key is already a hash value? do we still need to
  * scramble the key by shuffling its bits using something like phiMix?
  *
  * Fill factor, must be between (0 and 1)
  */
final class IntIntMap(initSize: Int, nValues: Int, fillFactor: Float = 0.75f) {
  /** Current map size */
  private var m_size: Int = _

  def size = m_size
}
