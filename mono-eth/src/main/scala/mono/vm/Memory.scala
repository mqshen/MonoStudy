package mono.vm

/**
  * Volatile memory with 256 bit address space.
  * Every mutating operation on a Memory returns a new updated copy of it.
  *
  * Related reading:
  * https://solidity.readthedocs.io/en/latest/frequently-asked-questions.html#what-is-the-memory-keyword-what-does-it-do
  * https://github.com/ethereum/go-ethereum/blob/master/core/vm/memory.go
  */
object Memory {
  def empty(): Memory = new Memory()
}

class Memory {

}
