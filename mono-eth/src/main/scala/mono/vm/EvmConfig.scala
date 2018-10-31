package mono.vm

import akka.util.ByteString
import mono.util.BlockchainConfig

object EvmConfig {
  /**
    * returns the evm config that should be used for given block
    */
  def forBlock(blockNumber: Long, blockchainConfig: BlockchainConfig): EvmConfig = {
    val transitionBlockToConfig = Map(
      blockchainConfig.frontierBlockNumber -> FrontierConfig
    )

    // highest transition block that is less/equal to `blockNumber`
    transitionBlockToConfig
      .filterKeys(_ <= blockNumber)
      .maxBy(_._1)
      ._2
  }

  val FrontierConfig = EvmConfig(
    feeSchedule = FeeSchedule.FrontierFeeSchedule,
    opCodes = OpCodes.FrontierOpCodes,
    exceptionalFailedCodeDeposit = false,
    subGasCapDivisor = None,
    chargeSelfDestructForNewAccount = false,
    maxContractSize = Int.MaxValue
  )
}

final case class EvmConfig(
  feeSchedule:                     FeeSchedule,
  opCodes:                         List[OpCode[_]],
  exceptionalFailedCodeDeposit:    Boolean,
  subGasCapDivisor:                Option[Long],
  chargeSelfDestructForNewAccount: Boolean,
  maxContractSize:                 Int) {

  private val byteToOpCode = {
    val ops = Array.ofDim[OpCode[_]](256)
    opCodes foreach { op =>
      ops(op.code.toInt & 0xFF) = op
      op.code.hashCode
    }
    ops
  }

  def getOpCode(code: Byte) = {
    Option(byteToOpCode(code.toInt & 0xFF))
  }

  /**
    * If the initialization code completes successfully, a final contract-creation cost is paid, the code-deposit cost,
    * proportional to the size of the created contract’s code. See YP equation (96)
    *
    * @param executionResultData Transaction code initialization result
    * @return Calculated gas cost
    */
  def calcCodeDepositCost(executionResultData: ByteString): Long =
    feeSchedule.G_codedeposit * executionResultData.size

}
object FeeSchedule {
  object FrontierFeeSchedule extends FrontierFeeSchedule
  class FrontierFeeSchedule extends FeeSchedule {
    override val G_zero = 0
    override val G_base = 2
    override val G_verylow = 3
    override val G_low = 5
    override val G_mid = 8
    override val G_high = 10
    override val G_balance = 20
    override val G_sload = 50
    override val G_jumpdest = 1
    override val G_sset = 20000
    override val G_sreset = 5000
    override val R_sclear = 15000
    override val R_selfdestruct = 24000
    override val G_selfdestruct = 0
    override val G_create = 32000
    override val G_codedeposit = 200
    override val G_call = 40
    override val G_callvalue = 9000
    override val G_callstipend = 2300
    override val G_newaccount = 25000
    override val G_exp = 10
    override val G_expbyte = 10
    override val G_memory = 3
    override val G_txcreate = 0
    override val G_txdatazero = 4
    override val G_txdatanonzero = 68
    override val G_transaction = 21000
    override val G_log = 375
    override val G_logdata = 8
    override val G_logtopic = 375
    override val G_sha3 = 30
    override val G_sha3word = 6
    override val G_copy = 3
    override val G_blockhash = 20
    override val G_extcode = 20
  }

  object HomesteadFeeSchedule extends HomesteadFeeSchedule
  class HomesteadFeeSchedule extends FrontierFeeSchedule {
    override val G_txcreate = 32000
  }

  object PostEIP150FeeSchedule extends PostEIP150FeeSchedule
  class PostEIP150FeeSchedule extends HomesteadFeeSchedule {
    override val G_sload = 200
    override val G_call = 700
    override val G_balance = 400
    override val G_selfdestruct = 5000
    override val G_extcode = 700
  }

  object PostEIP160FeeSchedule extends PostEIP160FeeSchedule
  class PostEIP160FeeSchedule extends PostEIP150FeeSchedule {
    override val G_expbyte = 50
  }
}
trait FeeSchedule {
  def G_zero: Long
  def G_base: Long
  def G_verylow: Long
  def G_low: Long
  def G_mid: Long
  def G_high: Long
  def G_balance: Long
  def G_sload: Long
  def G_jumpdest: Long
  def G_sset: Long
  def G_sreset: Long
  def R_sclear: Long
  def R_selfdestruct: Long
  def G_selfdestruct: Long
  def G_create: Long
  def G_codedeposit: Long
  def G_call: Long
  def G_callvalue: Long
  def G_callstipend: Long
  def G_newaccount: Long
  def G_exp: Long
  def G_expbyte: Long
  def G_memory: Long
  def G_txcreate: Long
  def G_txdatazero: Long
  def G_txdatanonzero: Long
  def G_transaction: Long
  def G_log: Long
  def G_logdata: Long
  def G_logtopic: Long
  def G_sha3: Long
  def G_sha3word: Long
  def G_copy: Long
  def G_blockhash: Long
  def G_extcode: Long
}