package mono.ledger

import mono.UInt256
import mono.vm.Storage

class TrieStorage extends Storage[TrieStorage] {
  override def store(offset: UInt256, value: UInt256) = ???

  override def load(offset: UInt256) = ???
}
