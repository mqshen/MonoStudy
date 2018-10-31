package mono.vm


object OpCodes {

  val LogOpCodes: List[OpCode[_]] = List(
  )

  val SwapOpCodes: List[OpCode[_]] = List(
  )

  val DupOpCodes: List[OpCode[_]] = List(
  )

  val PushOpCodes: List[OpCode[_]] = List(
  )

  val FrontierOpCodes: List[OpCode[_]] = LogOpCodes ++ SwapOpCodes ++ PushOpCodes ++ DupOpCodes ++ List()

  val HomesteadOpCodes: List[OpCode[_]] = FrontierOpCodes ++ List()
  val ByzantiumOpCodes: List[OpCode[_]] = HomesteadOpCodes ++ List( )
}
/**
  * Base class for all the opcodes of the EVM
  *
  * @tparam type of params
  * @param code Opcode byte representation
  * @param delta number of words to be popped from stack
  * @param alpha number of words to be pushed to stack
  */
sealed abstract class OpCode[P](val code: Byte, val delta: Int, val alpha: Int) {
  def this(code: Int, pop: Int, push: Int) = this(code.toByte, pop, push)

  final def execute[W <: WorldState[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    state
//    if (state.stack.size < delta) {
//      state.withError(StackUnderflow)
//    } else if (state.stack.size - delta + alpha > state.stack.maxSize) {
//      state.withError(StackOverflow)
//    } else {
//      val params = getParams(state)
//      if (state.error.isEmpty) { // error is checked during getParams
//        val constGas = constGasFn(state.config.feeSchedule)
//        val theGas = varGas(state, params)
//        val spendingGas = constGas + theGas
//        //println(s"spendingGas: $spendingGas, state.gas ${state.gas}, OOG? ${spendingGas > state.gas}")
//        // TODO since we use Long (signed number type) to calculate gas, how to
//        // make sure the value is always > 0 and < Long.MaxValue to avoid it becoming
//        // negative value
//        if (theGas < 0 || spendingGas < 0 || spendingGas > state.gas) {
//          state.withGas(0).withError(OutOfGas)
//        } else {
//          exec(state, params).spendGas(spendingGas)
//        }
//      } else {
//        state
//      }
//    }
  }

  protected def constGasFn(s: FeeSchedule): Long
  protected def varGas[W <: WorldState[W, S], S <: Storage[S]](state: ProgramState[W, S], params: P): Long
  protected def exec[W <: WorldState[W, S], S <: Storage[S]](state: ProgramState[W, S], params: P): ProgramState[W, S]
  protected def getParams[W <: WorldState[W, S], S <: Storage[S]](state: ProgramState[W, S]): P
}
