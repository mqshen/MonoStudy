package mono.domain

import mono.network.p2p.messages.PV62.{ BlockBody}

case class Block(header: BlockHeader, body: BlockBody) {
  override def toString: String = {
    s"""BlockHeader {
       | header: $header
       | body: $body
     """.stripMargin
  }
}


