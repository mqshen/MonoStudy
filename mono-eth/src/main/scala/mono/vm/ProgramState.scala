package mono.vm

import akka.util.ByteString
import mono.domain.{Address, TxLogEntry}
import mono.vm.ProgramState.ParallelRace

object ProgramState {
  trait ParallelRace
  case object OnAccount extends ParallelRace
  case object OnError extends ParallelRace
}

/**
  * Intermediate state updated with execution of each opcode in the program
  *
  * @param context the context which initiates the program
  * @param gas current gas for the execution
  * @param stack current stack
  * @param memory current memory
  * @param pc program counter - an index of the opcode in the program to be executed
  * @param returnData data to be returned from the program execution
  * @param gasRefund the amount of gas to be refunded after execution (not sure if a separate field is required)
  * @param addressesToDelete list of addresses of accounts scheduled to be deleted
  * @param halted a flag to indicate program termination
  * @param error indicates whether the program terminated abnormally
  */
final class ProgramState[W <: WorldState[W, S], S <: Storage[S]](val context: ProgramContext[W, S], val isDebugTraceEnabled: Boolean) {
  var gas: Long = context.startGas
  var world: W = context.world
  var addressesToDelete: Set[Address] = context.initialAddressesToDelete
  var addressesTouched: Set[Address] = context.initialAddressesTouched

  var pc: Int = 0
  var returnData: ByteString = ByteString()
  var gasRefund: Long = 0
  var txLogs: Vector[TxLogEntry] = Vector()
  private var _halted: Boolean = false
  var error: Option[ProgramError] = None
  private var _isRevert: Boolean = false


  private var _parallelRaceConditions = Set[ParallelRace]()

  val stack: Stack = Stack.empty()
  val memory: Memory = Memory.empty()

  def env: ExecEnv = context.env
  def program: Program = env.program


  def config: EvmConfig = context.config

  def parallelRaceConditions = _parallelRaceConditions
  def withParallelRaceCondition(race: ParallelRace) = {
    this._parallelRaceConditions += race
    this
  }

  def halt(): ProgramState[W, S] = {
    this._halted = true
    this
  }

  def isRevert = _isRevert
  def revert(): ProgramState[W, S] = {
    this._isRevert = true
    this
  }

  def withError(error: ProgramError): ProgramState[W, S] = {
    this.error = Some(error)
    this._halted = true
    this
  }
}

/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param env set of constants for the execution
  * @param targetAddress used for determining whether a precompiled contract is being called (potentially
  *                      different from the addresses defined in env)
  * @param startGas initial gas for the execution
  * @param world provides interactions with world state
  * @param config evm config
  * @param initialAddressesToDelete contains initial set of addresses to delete (from lower depth calls)
  */
final case class ProgramContext[W <: WorldState[W, S], S <: Storage[S]](
                                                                         env:                      ExecEnv,
                                                                         targetAddress:            Address,
                                                                         startGas:                 Long,
                                                                         world:                    W,
                                                                         config:                   EvmConfig,
                                                                         initialAddressesToDelete: Set[Address],
                                                                         initialAddressesTouched:  Set[Address],
                                                                         isStaticCall:             Boolean
                                                                       )