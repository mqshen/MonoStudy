package mono.domain

import akka.util.ByteString

final case class TxLogEntry(loggerAddress: Address, logTopics: Seq[ByteString], data: ByteString) {
  override def toString: String = {
    s"TxLogEntry(loggerAddress: $loggerAddress, logTopics: ${logTopics.map(e => mono.toHexString(e))}, data: ${mono.toHexString(data)})"
  }
}
