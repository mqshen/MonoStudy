package mono.util

import com.typesafe.config.{ConfigFactory, Config => TypesafeConfig}
import mono.UInt256
import mono.domain.Address
import mono.jsonrpc.JsonRpcController.JsonRpcConfig
import mono.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import mono.store.datasource.LeveldbConfig

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.util.Try

object Config {

  val config = ConfigFactory.load().getConfig("mono")

  val chainType = config.getString("chain-type")
  val datadir = config.getString("datadir") + "." + chainType

  val kesqueDir = datadir + "/" + config.getString("kesque-dir")

  object Network {
    private val networkConfig = config.getConfig("network")

    object Rpc extends JsonRpcHttpServerConfig with JsonRpcConfig {
      private val rpcConfig = networkConfig.getConfig("rpc")

      val enabled = rpcConfig.getBoolean("enabled")
      val interface = rpcConfig.getString("interface")
      val port = rpcConfig.getInt("port")

      val apis = {
        val providedApis = rpcConfig.getString("apis").split(",").map(_.trim.toLowerCase)
        val invalidApis = providedApis.diff(List("web3", "eth", "net", "personal"))
        require(invalidApis.isEmpty, s"Invalid RPC APIs specified: ${invalidApis.mkString(",")}")
        providedApis
      }
    }
  }

  trait DbConfig {
    val batchSize: Int
  }
  object Db extends DbConfig {

    private val dbConfig = config.getConfig("db")
    private val leveldbConfig = dbConfig.getConfig("leveldb")

    val batchSize = dbConfig.getInt("batch-size")

    object Leveldb extends LeveldbConfig {
      val path = datadir + "/" + leveldbConfig.getString("path")
      val createIfMissing = leveldbConfig.getBoolean("create-if-missing")
      val paranoidChecks = leveldbConfig.getBoolean("paranoid-checks")
      val verifyChecksums = leveldbConfig.getBoolean("verify-checksums")
      val cacheSize = leveldbConfig.getLong("cache-size")
    }
  }



  val isEth = chainType match {
    case "eth" => true
    case _     => false
  }

}

object BlockchainConfig {
  def apply(clientConfig: TypesafeConfig): BlockchainConfig = {
    val blockchainConfig = clientConfig.getConfig("blockchain")

    new BlockchainConfig {
      val frontierBlockNumber = blockchainConfig.getLong("frontier-block-number")

      val chainId = mono.hexDecode(blockchainConfig.getString("chain-id")).head
      val accountStartNonce = UInt256(blockchainConfig.getInt("account-start-nonce"))
      val customGenesisFileOpt = Try(blockchainConfig.getString("custom-genesis-file")).toOption

      val isDebugTraceEnabled = blockchainConfig.getBoolean("debug-trace-enabled")
    }
  }
}

trait BlockchainConfig {
  def frontierBlockNumber: Long

  def chainId: Byte

  def customGenesisFileOpt: Option[String]

  def accountStartNonce: UInt256

  def isDebugTraceEnabled: Boolean
}

object TxPoolConfig {
  def apply(etcClientConfig: com.typesafe.config.Config): TxPoolConfig = {
    val txPoolConfig = etcClientConfig.getConfig("txPool")

    new TxPoolConfig {
      val txPoolSize = txPoolConfig.getInt("tx-pool-size")
      val pendingTxManagerQueryTimeout = txPoolConfig.getDuration("pending-tx-manager-query-timeout").toMillis.millis
    }
  }
}

trait TxPoolConfig {
  val txPoolSize: Int
  val pendingTxManagerQueryTimeout: FiniteDuration
}

object MiningConfig {
  def apply(etcClientConfig: TypesafeConfig): MiningConfig = {
    val miningConfig = etcClientConfig.getConfig("mining")

    new MiningConfig {
      val coinbase = Address(miningConfig.getString("coinbase"))
      val blockCacheSize = miningConfig.getInt("block-cashe-size")
      val ommersPoolSize = miningConfig.getInt("ommers-pool-size")
      val activeTimeout = miningConfig.getDuration("active-timeout").toMillis.millis
      val ommerPoolQueryTimeout = miningConfig.getDuration("ommer-pool-query-timeout").toMillis.millis
    }
  }
}
trait MiningConfig {
  val ommersPoolSize: Int
  val blockCacheSize: Int
  val coinbase: Address
  val activeTimeout: FiniteDuration
  val ommerPoolQueryTimeout: FiniteDuration
}

object CacheConfig {
  def apply(clientConfig: TypesafeConfig): CacheConfig = {
    val cacheConfig = clientConfig.getConfig("cache")

    new CacheConfig {
      val cacheSize = cacheConfig.getInt("cache-size")
    }
  }
}

trait CacheConfig {
  val cacheSize: Int
}