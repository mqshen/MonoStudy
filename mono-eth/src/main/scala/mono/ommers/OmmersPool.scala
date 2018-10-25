package mono.ommers

import mono.domain.BlockHeader

object OmmersPool {
  final case class GetOmmers(blockNumber: Long)
  final case class Ommers(headers: List[BlockHeader])
}

class OmmersPool {

}
