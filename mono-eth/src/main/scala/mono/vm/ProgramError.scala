package mono.vm

/**
  * Marker trait for errors that may occur during program execution
  */
sealed trait ProgramError
case object OutOfGas extends ProgramError

final case class InvalidOpCode(code: Byte) extends ProgramError {
  override def toString: String =
    f"InvalidOpCode(0x${code.toInt & 0xff}%02x)"
}

