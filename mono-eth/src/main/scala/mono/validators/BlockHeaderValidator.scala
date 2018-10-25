package mono.validators

import mono.{Hash, UInt256}
import mono.domain.{Block, BlockHeader, Blockchain}
import mono.util.BlockchainConfig

object BlockHeaderValidator {
  trait I {
    def validate(blockHeader: BlockHeader, blockchain: Blockchain, validatingBlocks: Map[Hash, Block]): Either[BlockHeaderError, BlockHeader]
  }
}

class BlockHeaderValidator(blockchainConfig: BlockchainConfig) extends BlockHeaderValidator.I {
  override def validate(blockHeader: BlockHeader, blockchain: Blockchain, validatingBlocks: Map[Hash, Block]) = ???
}


sealed trait BlockHeaderError
object BlockHeaderError {
  case object HeaderParentNotFoundError extends BlockHeaderError
  case object HeaderExtraDataError extends BlockHeaderError
  case object HeaderTimestampError extends BlockHeaderError
  final case class HeaderDifficultyError(header: UInt256, calculated: UInt256) extends BlockHeaderError
  case object HeaderGasUsedError extends BlockHeaderError
  case object HeaderGasLimitError extends BlockHeaderError
  case object HeaderNumberError extends BlockHeaderError
  case object HeaderPoWError extends BlockHeaderError
}