package mono.domain

import mono.UInt256
import mono.util.BlockchainConfig

object DifficultyCalculator {
  private val FrontierTimestampDiffLimit = -99
  private val ExpDifficultyPeriod = 100000
  private val DifficultyBoundDivision = 2048
  private val MinimumDifficulty = UInt256(131072)
}
class DifficultyCalculator(blockchainConfig: BlockchainConfig) {
  import DifficultyCalculator._

  def calculateDifficulty(currHeader: BlockHeader, parentHeader: BlockHeader): UInt256 = UInt256(0)
    //calculateDifficulty(currHeader.number, currHeader.unixTimestamp, parentHeader)
  def calculateDifficulty(blockNumber: Long, blockTimestamp: Long, parentHeader: BlockHeader): UInt256 = {
    UInt256(0)
  }

}
