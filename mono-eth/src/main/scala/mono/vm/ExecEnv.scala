package mono.vm

import akka.util.ByteString
import mono.UInt256
import mono.domain.{Address, BlockHeader}

final case class ExecEnv(
                          ownerAddr:   Address,
                          callerAddr:  Address,
                          originAddr:  Address,
                          gasPrice:    UInt256,
                          inputData:   ByteString,
                          value:       UInt256,
                          program:     Program,
                          blockHeader: BlockHeader,
                          callDepth:   Int
                        )
