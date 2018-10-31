package kesque

import java.util.concurrent.locks.ReentrantReadWriteLock

object HashOffsets {
  type V = Int // unsigned int could be 2^32 = 4,294,967,296

}
final class HashOffsets(initSize: Int, nValues: Int = 1, fillFactor: Float = 0.75f) {
  import HashOffsets._

  private val singleValueMap = new IntIntMap(initSize, nValues, fillFactor)
  private val multipleValuesMap = new IntIntsMap(initSize, nValues, fillFactor)

  private val lock = new ReentrantReadWriteLock()
  private val readLock = lock.readLock
  private val writeLock = lock.writeLock

  def put(key: Int, value: V, valueIndex: Int): Array[V] = {
    withLock(writeLock) { () =>
      multipleValuesMap.get(key, valueIndex) match {
        case IntIntsMap.NO_VALUE =>
          singleValueMap.get(key, valueIndex) match {
            case IntIntMap.NO_VALUE => Array(singleValueMap.put(key, value, valueIndex))
            case existed =>
              singleValueMap.remove(key, valueIndex)
              multipleValuesMap.put(key, existed, valueIndex)
              multipleValuesMap.put(key, value, valueIndex)
          }
        case _ =>
          multipleValuesMap.put(key, value, valueIndex)
      }
    }
  }

  def size = singleValueMap.size + multipleValuesMap.size

  def get(key: Int, valueIndex: Int): Array[V] = {
     withLock(readLock) { () =>

      multipleValuesMap.get(key, valueIndex) match {
        case IntIntsMap.NO_VALUE =>
          singleValueMap.get(key, valueIndex) match {
            case IntIntMap.NO_VALUE => IntIntsMap.NO_VALUE
            case value              => Array(value)
          }
        case values => values
      }
    }
  }

  def replace(key: Int, toRemove: V, toPut: V, valueIndex: Int): Array[V] = {
    withLock(writeLock) { () =>
      if (toRemove == toPut) {
        Array(toPut)
      } else {
        multipleValuesMap.get(key, valueIndex) match {
          case IntIntsMap.NO_VALUE =>
            singleValueMap.get(key, valueIndex) match {
              case IntIntMap.NO_VALUE => Array(singleValueMap.put(key, toPut, valueIndex))
              case existed =>
                singleValueMap.remove(key, valueIndex)
                multipleValuesMap.put(key, existed, valueIndex)
                multipleValuesMap.replace(key, toRemove, toPut, valueIndex)
            }
          case _ =>
            multipleValuesMap.replace(key, toRemove, toPut, valueIndex)
        }
      }
    }
  }
}
