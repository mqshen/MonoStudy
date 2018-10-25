package mono.jsonrpc

import java.util.Date
import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import akka.actor.ActorSystem
import akka.event.Logging
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import mono.blockchain.sync.SyncService
import mono.domain.{Account, Address, BlockHeader}
import mono.mining.BlockGenerator
import mono.ommers.OmmersPool
import mono.store.AppStateStorage
import mono.{Hash, UInt256, crypto, util}
import mono.transactions.PendingTransactionsService

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object EthService {
  sealed trait BlockParam

  object BlockParam {
    final case class WithNumber(n: UInt256) extends BlockParam
    case object Latest extends BlockParam
    case object Pending extends BlockParam
    case object Earliest extends BlockParam
  }

  final case class SendRawTransactionRequest(data: ByteString)
  final case class SendRawTransactionResponse(transactionHash: Hash)

  final case class GetTransactionCountRequest(address: Address, block: BlockParam)
  final case class GetTransactionCountResponse(value: UInt256)

  final case class GetWorkRequest()
  final case class GetWorkResponse(powHeaderHash: Hash, dagSeed: ByteString, target: ByteString)
}

class EthService(
  blockGenerator:   BlockGenerator,
  appStateStorage:  AppStateStorage,
  miningConfig:     util.MiningConfig
)(implicit system: ActorSystem) {
  import EthService._
  private val log = Logging(system, this.getClass)

  def syncService = SyncService.proxy(system)

  val lastActive = new AtomicReference[Option[Date]](None)

  def pendingTransactionsService = PendingTransactionsService.proxy(system)

  def sendRawTransaction(req: SendRawTransactionRequest): ServiceResponse[SendRawTransactionResponse] = {
    import mono.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionDec

    Try(req.data.toArray.toSignedTransaction) match {
      case Success(signedTransaction) =>
        pendingTransactionsService ! PendingTransactionsService.AddOrOverrideTransaction(signedTransaction)
        Future.successful(Right(SendRawTransactionResponse(signedTransaction.hash)))
      case Failure(_) =>
        Future.successful(Left(JsonRpcErrors.InvalidRequest))
    }
  }

  def getWork(req: GetWorkRequest): ServiceResponse[GetWorkResponse] = {
    reportActive()
    import mono.mining.pow.PowCache._

    val blockNumber = appStateStorage.getBestBlockNumber() + 1
    getOmmersFromPool(blockNumber).zip(getTransactionsFromPool).map {
      case (ommers, pendingTxs) =>
        //try {
          blockGenerator.generateBlockForMining(blockNumber, pendingTxs.pendingTransactions.map(_.stx), ommers.headers, miningConfig.coinbase) match {
            case Right(pb) =>
              Right(GetWorkResponse(
                powHeaderHash = Hash(crypto.kec256(BlockHeader.getEncodedWithoutNonce(pb.block.header))),
                dagSeed = seedForBlock(pb.block.header.number),
                target = ByteString((UInt256.Modulus / pb.block.header.difficulty).bigEndianMag)
              ))
            case Left(err) =>
              log.error(s"unable to prepare block because of $err")
              Left(JsonRpcErrors.InternalError)
          }
//        } catch {
//          case err =>
//            err.printStackTrace()
//            log.error(s"unable to generate block because of $err" )
//            throw err
//        }
    }
  }

  private def getTransactionsFromPool = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (pendingTransactionsService ? PendingTransactionsService.GetPendingTransactions).mapTo[PendingTransactionsService.PendingTransactionsResponse]
      .recover {
        case ex =>
          //log.error("failed to get transactions, mining block with empty transactions list", ex)
          PendingTransactionsService.PendingTransactionsResponse(Nil)
      }
  }


  private def getOmmersFromPool(blockNumber: Long) = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (syncService ? OmmersPool.GetOmmers(blockNumber)).mapTo[OmmersPool.Ommers]
      .recover {
        case ex =>
          //log.error("failed to get ommer, mining block with empty ommers list", ex)
          OmmersPool.Ommers(Nil)
      }
  }

  private def reportActive() = {
    val now = new Date()
    lastActive.updateAndGet(new UnaryOperator[Option[Date]] {
      override def apply(e: Option[Date]): Option[Date] = {
        Some(now)
      }
    })
  }

  def getTransactionCount(req: GetTransactionCountRequest): ServiceResponse[GetTransactionCountResponse] = {
    Future.successful(Right(GetTransactionCountResponse(UInt256(1))))
      /*
    Future {
      Either()
      withAccount(req.address, req.block) { account =>
        GetTransactionCountResponse(UInt256(0))
        //GetTransactionCountResponse(account.nonce)
      }
    }
    */
  }


  /*
  private def withAccount[T](address: Address, blockParam: BlockParam)(f: Account => T): Either[JsonRpcError, T] = {
    resolveBlock(blockParam).map {
      case ResolvedBlock(block, _) =>
        f(blockchain.getAccount(address, block.header.number).getOrElse(Account.empty(blockchainConfig.accountStartNonce)))
    }
  }
  */

}
