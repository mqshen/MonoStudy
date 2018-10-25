package mono.store.trienode

import mono.Hash
import mono.util.SimpleMap

trait NodeKeyValueStorage extends SimpleMap[Hash, Array[Byte], NodeKeyValueStorage] {
  def tableName = ""
}
