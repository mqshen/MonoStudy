package mono.validators

import java.math.BigInteger

import mono.{Hash, UInt256}
import mono.domain.{Block, BlockHeader, Blockchain}
import mono.util.BlockchainConfig
import mono.validators.BlockHeaderError.{HeaderExtraDataError, HeaderParentNotFoundError}

object BlockHeaderValidator {
  trait I {
    def validate(blockHeader: BlockHeader, blockchain: Blockchain, validatingBlocks: Map[Hash, Block]): Either[BlockHeaderError, BlockHeader]
  }
}

class BlockHeaderValidator(blockchainConfig: BlockchainConfig) extends BlockHeaderValidator.I {

  /**
    * This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param blockHeader BlockHeader to validate.
    * @param blockchain  from where the header of the parent of the block will be fetched.
    */
  def validate(blockHeader: BlockHeader, blockchain: Blockchain, validatingBlocks: Map[Hash, Block]): Either[BlockHeaderError, BlockHeader] = {
    for {
      blockHeaderParent <- getBlockParentHeader(blockHeader, blockchain, validatingBlocks)
      _ <- validate(blockHeader, blockHeaderParent)
    } yield blockHeader
  }

  /**
    * This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param blockHeader  BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    */
  private def validate(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    for {
      _ <- validateExtraData(blockHeader)
      _ <- validateTimestamp(blockHeader, parentHeader)
      _ <- validateDifficulty(blockHeader, parentHeader)
      _ <- validateGasUsed(blockHeader)
      _ <- validateGasLimit(blockHeader, parentHeader)
      _ <- validateNumber(blockHeader, parentHeader)
      _ <- validatePoW(blockHeader)
    } yield blockHeader
  }

  /**
    * Validates [[mono.domain.BlockHeader.extraData]] length
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[mono.validators.BlockHeaderError.HeaderExtraDataError]] otherwise
    */
  private def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    println("need to implement validateExtraData")
    Right(blockHeader)
  }

  /**
    * Validates [[mono.domain.BlockHeader.unixTimestamp]] is greater than the one of its parent
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader  BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderTimestampError]] otherwise
    */
  private def validateTimestamp(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    println("need to implement validateTimestamp")
    Right(blockHeader)
  }
  /**
    * Validates [[mono.domain.BlockHeader.difficulty]] is correct
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderDifficultyError]] otherwise
    */
  private def validateDifficulty(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    println("need to implement validateDifficulty")
    Right(blockHeader)
  }

  /**
    * Validates [[mono.domain.BlockHeader.gasUsed]] is not greater than [[khipu.domain.BlockHeader.gasLimit]]
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderGasUsedError]] otherwise
    */
  private def validateGasUsed(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    println("need to implement validateGasUsed")
    Right(blockHeader)
  }

  /**
    * Validates [[mono.domain.BlockHeader.gasLimit]] follows the restrictions based on its parent gasLimit
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderGasLimitError]] otherwise
    */
  private def validateGasLimit(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    println("need to implement validateGasLimit")
    Right(blockHeader)
  }

  /**
    * Validates [[mono.domain.BlockHeader.number]] is the next one after its parents number
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderNumberError]] otherwise
    */
  private def validateNumber(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    println("need to implement validateNumber")
    Right(blockHeader)
  }

  /**
    * Validates [[mono.domain.BlockHeader.nonce]] and [[mono.domain.BlockHeader.mixHash]] are correct
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderPoWError]] otherwise
    */
  //FIXME: Simple PoW validation without using DAG [EC-88]
  private def validatePoW(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeader] = {
    println("need to implement validatePoW")
    Right(blockHeader)
  }

  /**
    * Retrieves the header of the parent of a block from the Blockchain, if it exists.
    *
    * @param blockHeader BlockHeader whose parent we want to fetch.
    * @param blockchain where the header of the parent of the block will be fetched.
    * @return the BlockHeader of the parent if it exists, an [[mono.validators.BlockHeaderError.HeaderParentNotFoundError]] otherwise
    */
  private def getBlockParentHeader(blockHeader: BlockHeader, blockchain: Blockchain, validatingBlocks: Map[Hash, Block]): Either[BlockHeaderError, BlockHeader] = {
    validatingBlocks.get(blockHeader.parentHash).map(_.header).orElse(blockchain.getBlockHeaderByHash(blockHeader.parentHash)) match {
      case Some(blockParentHeader) => Right(blockParentHeader)
      case None                    => Left(HeaderParentNotFoundError)
    }
  }
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