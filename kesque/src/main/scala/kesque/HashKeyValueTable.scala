package kesque

import java.nio.ByteBuffer
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import kafka.utils.Logging
import kesque.HashKeyValueTable.{bytesToInt, fetchMaxBytesInLoadOffsets}
import org.apache.kafka.common.record.CompressionType

object HashKeyValueTable {
  val fetchMaxBytesInLoadOffsets = 100 * 1024 * 1024 // 100M
  val defaultFetchMaxBytes = 4 * 1024 // 4K the size of SSD block

  def intToBytes(v: Int) = ByteBuffer.allocate(4).putInt(v).array
  def bytesToInt(v: Array[Byte]) = ByteBuffer.wrap(v).getInt
}

class HashKeyValueTable(
  topics:          Array[String],
  db:              Kesque,
  withTimeToKey:   Boolean,
  fetchMaxBytes:   Int             = HashKeyValueTable.defaultFetchMaxBytes,
  compressionType: CompressionType = CompressionType.NONE,
  cacheSize:       Int             = 10000
) extends Logging {

  private val hashOffsets = new HashOffsets(200, topics.length)
  /* time to key table, should be the first topic to initially create it */
  private var timeIndex = Array.ofDim[Array[Byte]](200)

  private val caches = Array.ofDim[FIFOCache[Hash, (TVal, Int)]](topics.length)

  private val indexTopics = topics map indexTopic

  private def indexTopic(topic: String) = topic + "_idx"

  private val lock = new ReentrantReadWriteLock()
  private val readLock = lock.readLock
  private val writeLock = lock.writeLock

  def withLock[T <: Lock, V](r: => T)(f: () => V): V = {
    val lock: T = r
    try {
      lock.lock()
      f()
    } finally {
      lock.unlock()
    }
  }

  def getKeyByTime(timestamp: Long): Option[Array[Byte]] = {
    withLock(readLock) { () =>
      if (!withTimeToKey) {
        None
      } else {
        if (timestamp >= 0 && timestamp < timeIndex.length) {
          Option(timeIndex(timestamp.toInt))
        } else {
          None
        }
      }
    }
  }


  def putTimeToKey(timestamp: Long, key: Array[Byte]): Unit = {
    withLock(writeLock) { () =>
      if (timestamp > timeIndex.length - 1) {
        val newArr = Array.ofDim[Array[Byte]]((timeIndex.length * 1.2).toInt)
        System.arraycopy(timeIndex, 0, newArr, 0, timeIndex.length)
        timeIndex = newArr
      }
      timeIndex(timestamp.toInt) = key
    }
  }

  def write(kvs: Iterable[TKeyVal], topic: String) = {
    //TODO
    println("hashKeyValueTable need to implement")
  }

  private class LoadIndexesTask(valueIndex: Int, topic: String) extends Thread {
    override def run() {
      loadOffsets(valueIndex)
    }
  }

  private def loadOffsets(valueIndex: Int) {
    val indexTopic = indexTopics(valueIndex)

    info(s"Loading index of ${topics(valueIndex)}")
    val start = System.currentTimeMillis

    var count = 0
    db.iterateOver(indexTopic, 0, fetchMaxBytesInLoadOffsets) {
      case (offset, TKeyVal(hashCode, recordOffset, timestamp)) =>
        if (hashCode != null && recordOffset != null) {
          hashOffsets.put(bytesToInt(hashCode), bytesToInt(recordOffset), valueIndex)
          count += 1
        }
    }

    info(s"Loaded index of ${topics(valueIndex)} in ${System.currentTimeMillis - start} ms, count $count, size ${hashOffsets.size}")
  }

  loadIndexes()

  private def loadIndexes() {
    var tasks = List[Thread]()
    var n = 0
    while (n < topics.length) {
      val topic = topics(n)
      caches(n) = new FIFOCache[Hash, (TVal, Int)](cacheSize)

      tasks = new LoadIndexesTask(n, topic) :: tasks

      n += 1
    }

    val timeIndexTask = if (withTimeToKey) {
      List(new Thread() {
        override def run() {
          loadTimeIndex
        }
      })
    } else {
      Nil
    }

    timeIndexTask ::: tasks foreach { _.start() }
    timeIndexTask ::: tasks foreach { _.join() }
  }

  private def loadTimeIndex() {
    val topic = topics(0)

    info(s"Loading time index from $topic")
    val start = System.currentTimeMillis

    var count = 0
    db.iterateOver(topic, 0, fetchMaxBytesInLoadOffsets) {
      case (offset, TKeyVal(key, value, timestamp)) =>
        if (key != null && value != null) {
          putTimeToKey(timestamp, key)
          count += 1
        }
    }

    info(s"Loaded time index from $topic in ${System.currentTimeMillis - start} ms, count $count")
  }
}
