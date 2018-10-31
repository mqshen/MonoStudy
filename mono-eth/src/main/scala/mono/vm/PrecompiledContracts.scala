package mono.vm

import akka.util.ByteString
import mono.domain.Address

object PrecompiledContracts {

  val EcDsaRecAddr = Address(1)
  val Sha256Addr = Address(2)
  val Rip160Addr = Address(3)
  val IdAddr = Address(4)
  val ModExpAddr = Address(5)
  val AltBN128AddAddr = Address(6)
  val AltBN128MulAddr = Address(7)
  val AltBN128PairingAddr = Address(8)

  def getContractForAddress(address: Address, config: EvmConfig): Option[PrecompiledContract] = address match {
    case EcDsaRecAddr                         => Some(ECRecovery)
    case _                                    => None
  }

  sealed trait PrecompiledContract {
    def run[W <: WorldState[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
      null
    }
  }
  object ECRecovery extends PrecompiledContract {

  }
}
class PrecompiledContracts {

}
