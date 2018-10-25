package mono.transactions

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import akka.cluster.client.ClusterClientReceptionist
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.Publish
import akka.cluster.singleton.{ClusterSingletonManager, ClusterSingletonManagerSettings, ClusterSingletonProxy, ClusterSingletonProxySettings}
import mono.BroadcastTransactions
import mono.domain.SignedTransaction
import mono.util.TxPoolConfig

object PendingTransactionsService {
  def props(txPoolConfig: TxPoolConfig): Props =
    Props(classOf[PendingTransactionsService], txPoolConfig)

  val name = "pendingTxService"
  val managerName = "khipuSingleton-" + name
  val managerPath = "/user/" + managerName
  val proxyName = "monoSingletonProxy-" + name
  val proxyPath = "/user/" + proxyName

  def start(system: ActorSystem, role: Option[String],
            txPoolConfig: TxPoolConfig): ActorRef = {
    val settings = ClusterSingletonManagerSettings(system).withRole(role).withSingletonName(name)
    system.actorOf(
      ClusterSingletonManager.props(
        singletonProps = props(txPoolConfig),
        terminationMessage = PoisonPill,
        settings = settings
      ), name = managerName
    )
  }

  def startProxy(system: ActorSystem, role: Option[String]): ActorRef = {
    val settings = ClusterSingletonProxySettings(system).withRole(role).withSingletonName(name)
    val proxy = system.actorOf(
      ClusterSingletonProxy.props(
        singletonManagerPath = managerPath,
        settings = settings
      ), name = proxyName
    )
    ClusterClientReceptionist(system).registerService(proxy)
    proxy
  }

  def proxy(system: ActorSystem) = system.actorSelection(proxyPath)

  final case class AddTransactions(signedTransactions: List[SignedTransaction])
  final case class AddOrOverrideTransaction(signedTransaction: SignedTransaction)

  case object GetPendingTransactions
  final case class PendingTransactionsResponse(pendingTransactions: Seq[PendingTransaction])

  final case class PendingTransaction(stx: SignedTransaction, addTimestamp: Long)
}


final class PendingTransactionsService(txPoolConfig: TxPoolConfig) extends Actor with ActorLogging {
  import PendingTransactionsService._

  /**
    * stores all pending transactions
    */
  private var pendingTransactions: List[PendingTransaction] = Nil

  val mediator = DistributedPubSub(context.system).mediator

  override def receive: Receive = {
    case AddTransactions(signedTransactions) =>
      addTransactions(signedTransactions)
    case AddOrOverrideTransaction(newStx) =>
      addOrOverrideTransaction(newStx)
    case GetPendingTransactions =>
      sender() ! PendingTransactionsResponse(pendingTransactions)
  }

  def addTransactions(signedTransactions: List[SignedTransaction]) {
    val transactionsToAdd = signedTransactions.filterNot(stx => pendingTransactions.contains(stx.hash))
    if (transactionsToAdd.nonEmpty) {
      val timestamp = System.currentTimeMillis
      pendingTransactions = (transactionsToAdd.map(PendingTransaction(_, timestamp)) ::: pendingTransactions).take(txPoolConfig.txPoolSize)

      broadcastNewTransactions(transactionsToAdd)
    }
  }

  def addOrOverrideTransaction(newTx: SignedTransaction) {
    val txsWithoutObsoletes = pendingTransactions.filterNot { ptx =>
      ptx.stx.sender == newTx.sender && ptx.stx.tx.nonce == newTx.tx.nonce
    }

    val timestamp = System.currentTimeMillis()
    pendingTransactions = (PendingTransaction(newTx, timestamp) :: txsWithoutObsoletes).take(txPoolConfig.txPoolSize)

    broadcastNewTransactions(List(newTx))
  }

  def broadcastNewTransactions(signedTransactions: Seq[SignedTransaction]) = {
    val ptxHashs = pendingTransactions.map(_.stx.hash).toSet
    val txsToNotify = signedTransactions.filter(tx => ptxHashs.contains(tx.hash)) // signed transactions that are still pending
    if (txsToNotify.nonEmpty) {
      mediator ! Publish(mono.TxTopic, BroadcastTransactions(txsToNotify))
    }
  }
}