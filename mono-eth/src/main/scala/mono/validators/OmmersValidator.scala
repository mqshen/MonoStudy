package mono.validators

import mono.Hash
import mono.domain.{Block, BlockHeader, Blockchain}
import mono.util.BlockchainConfig

object OmmersValidator {
  trait I {
    def validate(blockNumber: Long, ommers: Seq[BlockHeader], blockchain: Blockchain, validatingBlocks: Map[Hash, Block]): Either[OmmersError, Unit]
  }

  sealed trait OmmersError
  object OmmersError {
    case object OmmersLengthError extends OmmersError
    case object OmmersNotValidError extends OmmersError
    case object OmmersUsedBeforeError extends OmmersError
    case object OmmersAncestorsError extends OmmersError
    case object OmmersDuplicatedError extends OmmersError
  }
}
class OmmersValidator(blockchainConfig: BlockchainConfig) extends OmmersValidator.I {
  override def validate(blockNumber: Long, ommers: Seq[BlockHeader], blockchain: Blockchain, validatingBlocks: Map[Hash, Block]) = ???
}
