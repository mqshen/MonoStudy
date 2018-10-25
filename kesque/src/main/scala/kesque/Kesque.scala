package kesque

import java.util.Properties

import kafka.server.QuotaFactory.UnboundedQuota
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.record.CompressionType
import org.apache.kafka.common.requests.FetchRequest.PartitionData

import scala.collection.mutable

final class Kesque(props: Properties) {
  private val kafkaServer = KafkaServer.start(props)
  private val replicaManager = kafkaServer.replicaManager

  private val topicToTable = mutable.Map[String, HashKeyValueTable]()

  def getTimedTable(topics: Array[String], fetchMaxBytes: Int = 4096, compressionType: CompressionType = CompressionType.NONE, cacheSize: Int = 10000) = {
    topicToTable.getOrElseUpdate(topics.mkString(","), new HashKeyValueTable(topics, this, true, fetchMaxBytes, compressionType, cacheSize))
  }

  private[kesque] def read(topic: String, fetchOffset: Long, fetchMaxBytes: Int) = {
    val partition = new TopicPartition(topic, 0)
    val partitionData = new PartitionData(fetchOffset, 0L, fetchMaxBytes)

    replicaManager.readFromLocalLog(
      replicaId = 0,
      fetchOnlyFromLeader = true,
      readOnlyCommitted = false,
      fetchMaxBytes = fetchMaxBytes,
      hardMaxBytesLimit = false, // read at lease one message even exceeds the fetchMaxBytes
      readPartitionInfo = List((partition, partitionData)),
      quota = UnboundedQuota,
      isolationLevel = org.apache.kafka.common.requests.IsolationLevel.READ_COMMITTED
    )
  }

  /**
    * @param topic
    * @param fetchOffset
    * @param op: action applied on (offset, key, value)
    */
  private[kesque] def iterateOver(topic: String, fetchOffset: Long = 0L, fetchMaxBytes: Int)(op: (Long, TKeyVal) => Unit) = {
    var offset = fetchOffset
    var nRead = 0
    do {
      readOnce(topic, offset, fetchMaxBytes)(op) match {
        case (n, o) =>
          nRead = n
          offset = o + 1
      }
    } while (nRead > 0)
  }

  private[kesque] def readOnce(topic: String, fetchOffset: Long, fetchMaxBytes: Int)(op: (Long, TKeyVal) => Unit) = {
    val (topicPartition, result) = read(topic, fetchOffset, fetchMaxBytes).head
    val recs = result.info.records.records.iterator
    var i = 0
    var lastOffset = fetchOffset
    while (recs.hasNext) {
      val rec = recs.next
      val key = if (rec.hasKey) kesque.getBytes(rec.key) else null
      val value = if (rec.hasValue) kesque.getBytes(rec.value) else null
      val timestamp = rec.timestamp
      val offset = rec.offset
      op(offset, TKeyVal(key, value, timestamp))

      lastOffset = offset
      i += 1
    }

    (i, lastOffset)
  }

}

final case class TKeyVal(key: Array[Byte], value: Array[Byte], timestamp: Long = -1L)
final case class TVal(value: Array[Byte], timestamp: Long)