package mono.validators

trait Validators {
  val blockValidator: BlockValidator
  val blockHeaderValidator: BlockHeaderValidator.I
  val ommersValidator: OmmersValidator.I
  val signedTransactionValidator: SignedTransactionValidator
}
