package mono.vm

object ProgramState {
  trait ParallelRace
  case object OnAccount extends ParallelRace
  case object OnError extends ParallelRace
}
