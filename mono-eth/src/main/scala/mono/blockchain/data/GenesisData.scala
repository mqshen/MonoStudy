package mono.blockchain.data

import akka.util.ByteString
import mono.Hash

final case class GenesisData(
  parentHash: Hash,
  coinbase:   ByteString,
  difficulty: String,
  gasLimit:   String,
  timestamp:  String,
  extraData:  ByteString,
  mixHash:    Hash,
  nonce:      ByteString,
  alloc:      Map[String, AllocAccount]
)

final case class AllocAccount(balance: String)
