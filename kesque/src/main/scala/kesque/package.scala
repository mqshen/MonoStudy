import java.nio.ByteBuffer
import java.util.concurrent.locks.Lock

import org.apache.kafka.common.record._
import org.apache.kafka.common.utils.ByteBufferOutputStream

package object kesque {


  def withLock[T <: Lock, V](r: => T)(f: () => V): V = {
    val lock: T = r
    try {
      lock.lock()
      f()
    } finally {
      lock.unlock()
    }
  }
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

  def buildRecords(compressionType: CompressionType, initialOffset: Long, records: SimpleRecord*): MemoryRecords = myBuildRecords(
    RecordBatch.CURRENT_MAGIC_VALUE, initialOffset, compressionType,
    TimestampType.CREATE_TIME, 0, 0,
    0, RecordBatch.NO_PARTITION_LEADER_EPOCH, isTransactional = false,
    records: _*
  )

  def myBuildRecords(magic: Byte, initialOffset: Long, compressionType: CompressionType,
                   timestampType: TimestampType, producerId: Long, producerEpoch: Short,
                   baseSequence: Int, partitionLeaderEpoch: Int, isTransactional: Boolean,
                   records: SimpleRecord*): MemoryRecords = {
    if (records.isEmpty) {
      MemoryRecords.EMPTY
    } else {
      import scala.collection.JavaConverters._
      val sizeEstimate = AbstractRecords.estimateSizeInBytes(magic, compressionType, records.asJava)
      val bufferStream = new ByteBufferOutputStream(sizeEstimate)
      val logAppendTime = timestampType match {
        case TimestampType.LOG_APPEND_TIME => System.currentTimeMillis()
        case _                             => RecordBatch.NO_TIMESTAMP
      }

      val builder = new MemoryRecordsBuilder(bufferStream, magic, compressionType, timestampType,
        initialOffset, logAppendTime, producerId, producerEpoch, baseSequence, isTransactional, false,
        partitionLeaderEpoch, sizeEstimate)

      records foreach builder.append

      builder.build()
    }
  }
}
