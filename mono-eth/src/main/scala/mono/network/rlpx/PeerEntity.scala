package mono.network.rlpx

import akka.actor.{Actor, ActorLogging, Timers}
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.Subscribe
import mono.{BroadcastTransactions, Hash}
import mono.domain.SignedTransaction

class PeerEntity(peer: Peer) extends Actor with Timers with ActorLogging {
  /**
    * stores information which tx hashes are "known" by which peers Seq[peerId]
    */
  private var knownTransactions = Set[Hash]()

  val mediator = DistributedPubSub(context.system).mediator
  mediator ! Subscribe(mono.TxTopic, self)
  mediator ! Subscribe(mono.NewBlockTopic, self)

  override def receive = broadcastBehavior

  def broadcastBehavior: Receive = {
    case BroadcastTransactions(transactions) => // from mono.TxTopic
      val transactionsNonKnown = transactions.filterNot(isTxKnown)
      if (transactionsNonKnown.nonEmpty) {
        // TODO broadcast it only after fast-sync done? otherwise seems will cause
        // too busy to request/respond
        //self ! PeerEntity.MessageToPeer(peerId, SignedTransactions(transactionsNonKnown))
        setTxKnown(transactionsNonKnown)
      }
  }

  private def isTxKnown(transactions: SignedTransaction): Boolean =
    knownTransactions.contains(transactions.hash)

  private def setTxKnown(transactions: Seq[SignedTransaction]) {
    knownTransactions ++= transactions.map(_.hash)
  }

}
