import akka.util.ByteString
import mono.domain.SignedTransaction
import org.spongycastle.util.encoders.Hex

package object mono {
  type UInt256 = UInt256_biginteger.UInt256
  val UInt256 = UInt256_biginteger.UInt256

  def toHexString(bytes: Array[Byte]): String = Hex.toHexString(bytes)
  def toHexString(bytes: ByteString): String = Hex.toHexString(bytes.toArray)
  def hexDecode(hexString: String): Array[Byte] = Hex.decode(hexString)

  val TxTopic = "mono.tx"
  val NewBlockTopic = "mono.newblock"

  final case class BroadcastTransactions(transactions: Seq[SignedTransaction])

  sealed trait Log[+T] { def value: T }
  sealed trait Changed[+T] extends Log[T]
  final case class Deleted[T](value: T) extends Changed[T]
  final case class Updated[T](value: T) extends Changed[T]
  final case class Original[T](value: T) extends Log[T]

}