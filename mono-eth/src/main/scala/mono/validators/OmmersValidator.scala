package mono.validators

import mono.Hash
import mono.domain.{Block, BlockHeader, Blockchain}
import mono.util.BlockchainConfig
import mono.validators.OmmersValidator.OmmersError
import mono.validators.OmmersValidator.OmmersError._

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
  val blockHeaderValidator = new BlockHeaderValidator(blockchainConfig)

  val OmmerGenerationLimit: Int = 6 //Stated on section 11.1, eq. (143) of the YP
  val OmmerSizeLimit: Int = 2

  override def validate(blockNumber: Long, ommers: Seq[BlockHeader], blockchain: Blockchain, validatingBlocks: Map[Hash, Block]) = {
    for {
      _ <- validateOmmersLength(ommers)
      _ <- validateDuplicatedOmmers(ommers)
      _ <- validateOmmersHeaders(ommers, blockchain, validatingBlocks)
      searchingMap = validatingBlocks.map(x => x._2.header.number -> x._2).toMap
      _ <- validateOmmersAncestors(blockNumber, ommers, blockchain, searchingMap)
      _ <- validateOmmersNotUsed(blockNumber, ommers, blockchain, searchingMap)
    } yield ()
  }

  /**
    * Validates ommers length
    * based on validations stated in section 11.1 of the YP
    *
    * @param ommers         The list of ommers to validate
    * @return ommers if valid, an [[OmmersLengthError]] otherwise
    */
  private def validateOmmersLength(ommers: Seq[BlockHeader]): Either[OmmersError, Unit] = {
    if (ommers.length <= OmmerSizeLimit) {
      Right(())
    } else {
      Left(OmmersLengthError)
    }
  }

  /**
    * Validates that there are no duplicated ommers
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param ommers         The list of ommers to validate
    * @return ommers if valid, an [[OmmersDuplicatedError]] otherwise
    */
  private def validateDuplicatedOmmers(ommers: Seq[BlockHeader]): Either[OmmersError, Unit] = {
    if (ommers.distinct.length == ommers.length) {
      Right(())
    } else {
      Left(OmmersDuplicatedError)
    }
  }

  /**
    * Validates that each ommer's header is valid
    * based on validations stated in section 11.1 of the YP
    *
    * @param ommers         The list of ommers to validate
    * @param blockchain     from where the ommer's parents will be obtained
    * @return ommers if valid, an [[OmmersNotValidError]] otherwise
    */
  private def validateOmmersHeaders(ommers: Seq[BlockHeader], blockchain: Blockchain, validatingBlocks: Map[Hash, Block]): Either[OmmersError, Unit] = {
    if (ommers.forall(blockHeaderValidator.validate(_, blockchain, validatingBlocks).isRight)) {
      Right(())
    } else {
      Left(OmmersNotValidError)
    }
  }

  /**
    * Validates that each ommer is not too old and that it is a sibling as one of the current block's ancestors
    * based on validations stated in section 11.1 of the YP
    *
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param blockchain     from where the ommer's parents will be obtained
    * @return ommers if valid, an [[OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersAncestors(blockNumber: Long, ommers: Seq[BlockHeader], blockchain: Blockchain, validatingBlocks: Map[Long, Block]): Either[OmmersError, Unit] = {
    println("need implement validateOmmersAncestors")
    Right(())
  }

  /**
    * Validates that each ommer was not previously used
    * based on validations stated in the white paper (https://github.com/ethereum/wiki/wiki/White-Paper)
    *
    * @param blockNumber    The number of the block to which the ommers belong
    * @param ommers         The list of ommers to validate
    * @param blockchain     from where the ommer's parents will be obtained
    * @return ommers if valid, an [[OmmersUsedBeforeError]] otherwise
    */
  private def validateOmmersNotUsed(blockNumber: Long, ommers: Seq[BlockHeader], blockchain: Blockchain, validatingBlocks: Map[Long, Block]): Either[OmmersError, Unit] = {
    println("need implement validateOmmersNotUsed")
    Right(())
  }
}
