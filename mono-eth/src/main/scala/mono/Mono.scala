package mono

import akka.actor.{ActorSystem, CoordinatedShutdown}
import akka.event.Logging
import mono.blockchain.data.GenesisDataLoader
import mono.jsonrpc.http.JsonRpcHttpServer
import mono.jsonrpc.{EthService, JsonRpcController}
import mono.mining.BlockGenerator
import mono.service.ServiceBoard
import mono.transactions.PendingTransactionsService

object Mono {
  implicit val system = ActorSystem("mono")

  private val log = Logging(system, this.getClass)

  val serviceBoard = ServiceBoard(system)

  def main(args: Array[String]) {
    CoordinatedShutdown(system).addJvmShutdownHook {
      log.info("mono db stopping ...")

      log.info("mono db stopped")
    }

    val genesisDataLoader = new GenesisDataLoader(serviceBoard.storages.dataSource, serviceBoard.blockchain, serviceBoard.blockchainConfig, serviceBoard.db, serviceBoard.storages.accountNodeStorageFor)
    genesisDataLoader.loadGenesisData()

    startPendingTxService()

    startJsonRpcServer()
  }

  def startPendingTxService() {
    PendingTransactionsService.start(system, Some("entity"), serviceBoard.txPoolConfig)
    PendingTransactionsService.startProxy(system, Some("entity"))
  }

  def startJsonRpcServer(): Unit = {
    val jsonRpcHttpServerConfig = util.Config.Network.Rpc

    val blockGenerator = new BlockGenerator(serviceBoard.blockchain, serviceBoard.blockchainConfig, serviceBoard.miningConfig, serviceBoard.ledger, serviceBoard.validators)
    val ethService = new EthService(blockGenerator,
      serviceBoard.storages.appStateStorage,
      serviceBoard.miningConfig)

    val jsonRpcController = new JsonRpcController(ethService, jsonRpcHttpServerConfig)
    val jsonRpcHttpServer = new JsonRpcHttpServer(jsonRpcController, jsonRpcHttpServerConfig)

    jsonRpcHttpServer.run()
  }
}

class Mono
