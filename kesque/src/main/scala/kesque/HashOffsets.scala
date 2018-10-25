package kesque

object HashOffsets {
  type V = Int // unsigned int could be 2^32 = 4,294,967,296

}
final class HashOffsets(initSize: Int, nValues: Int = 1, fillFactor: Float = 0.75f) {
  import HashOffsets._

  private val singleValueMap = new IntIntMap(initSize, nValues, fillFactor)
  private val multipleValuesMap = new IntIntsMap(initSize, nValues, fillFactor)

  def put(key: Int, value: V, valueIndex: Int): Array[V] = {
    //TODO
    println("need to implement HashOffsets")
    Array.empty
  }

  def size = singleValueMap.size + multipleValuesMap.size
}
