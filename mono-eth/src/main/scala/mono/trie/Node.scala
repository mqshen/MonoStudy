package mono.trie

import mono.{rlp, trie}
import mono.rlp._

object Node {
  import mono.rlp.RLPImplicitConversions._
  import mono.rlp.RLPImplicits._

  private[trie] val ListSize: Byte = 17
  private val PairSize: Byte = 2

  object nodeEnc extends RLPEncoder[Node] {
    override def encode(obj: Node): RLPEncodeable = obj match {
      case LeafNode(key, value) =>
        RLPList(HexPrefix.encode(nibbles = key, isLeaf = true), value)

      case ExtensionNode(sharedKey, next) =>
        RLPList(HexPrefix.encode(nibbles = sharedKey, isLeaf = false), next match {
          case Right(node) => encode(node)
          case Left(bytes) => bytes
        })

      case BranchNode(children, terminator) =>
        val childrenEncoded = children.map {
          case Some(Right(node)) => encode(node)
          case Some(Left(bytes)) => RLPValue(bytes)
          case None              => RLPValue(Array.emptyByteArray)
        }
        val encoded = Array.ofDim[RLPEncodeable](childrenEncoded.length + 1)
        System.arraycopy(childrenEncoded, 0, encoded, 0, childrenEncoded.length)
        encoded(encoded.length - 1) = RLPValue(terminator.getOrElse(Array.emptyByteArray))

        RLPList(encoded: _*)
    }
  }

  object nodeDec extends RLPDecoder[Node] {
    override def decode(rlp: RLPEncodeable): Node = rlp match {
      case RLPList(xs @ _*) =>
        val items = xs.toArray
        items.length match {
          case ListSize =>
            val childrenLength = items.length - 1
            val parsedChildren = Array.ofDim[Option[Either[Array[Byte], Node]]](childrenLength)
            val last = items(items.length - 1)
            var i = 0
            while (i < childrenLength) {
              val child = items(i) match {
                case list: RLPList =>
                  Some(Right(decode(list)))
                case RLPValue(bytes) =>
                  if (bytes.length == 0) {
                    None
                  } else {
                    Some(Left(bytes))
                  }
              }
              parsedChildren(i) = child
              i += 1
            }

            val teminatorBytes = fromEncodeable[Array[Byte]](last)
            val terminator = if (teminatorBytes.length == 0) {
              None
            } else {
              Some(teminatorBytes)
            }
            BranchNode(
              parsedChildren,
              terminator
            )
          case PairSize =>
            HexPrefix.decode(items(0)) match {
              case (key, true) =>
                LeafNode(key, items(1))

              case (key, false) =>
                ExtensionNode(
                  key,
                  items(1) match {
                    case list: RLPList   => Right(decode(list))
                    case RLPValue(bytes) => Left(bytes)
                  }
                )
            }
          case _ => throw new RuntimeException("Invalid Node")
        }
      case x =>
        println(x)
        throw new RuntimeException("Invalid Node")
    }
  }
}
/**
  * When store node to storage, the key is node.hash, the value is node.encoded.
  * When node is changed, the key will always change too. Thus, to a specified
  * key, the value should be null or the same node
  */
sealed trait Node {
  lazy val encoded: Array[Byte] = rlp.encode[Node](this)(Node.nodeEnc)
  lazy val hash: Array[Byte] = trie.toHash(encoded)

  final def capped: Array[Byte] = if (encoded.length < 32) encoded else hash
}


final case class LeafNode(key: Array[Byte], value: Array[Byte]) extends Node

object ExtensionNode {
  /**
    * This function creates a new ExtensionNode with next parameter as its node pointer
    *
    * @param sharedKey of the new ExtensionNode.
    * @param next      to be inserted as the node pointer (and hashed if necessary).
    * @return a new BranchNode.
    */
  def apply(sharedKey: Array[Byte], next: Node): ExtensionNode = {
    val nextCapped = next.capped
    ExtensionNode(sharedKey, if (nextCapped.length == 32) Left(nextCapped) else Right(next))
  }
}
final case class ExtensionNode(sharedKey: Array[Byte], next: Either[Array[Byte], Node]) extends Node

object BranchNode {
  /**
    * This function creates a new terminator BranchNode having only a value associated with it.
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withValueOnly(terminator: Array[Byte]): BranchNode = {
    BranchNode(Array.fill(Node.ListSize - 1)(None), Some(terminator))
  }

  /**
    * This function creates a new BranchNode having only one child associated with it (and optionaly a value).
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param position   of the BranchNode children where the child should be inserted.
    * @param child      to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withSingleChild(position: Byte, child: Node, terminator: Option[Array[Byte]]): BranchNode = {
    val children = Array.fill[Option[Either[Array[Byte], Node]]](Node.ListSize - 1)(None)
    val childCapped = child.capped
    children(position) = Some(if (childCapped.length == 32) Left(childCapped) else Right(child))
    BranchNode(children, terminator)
  }

  /**
    * This function creates a new BranchNode having only one child associated with it (and optionaly a value).
    * This new BranchNode will be temporarily in an invalid state.
    *
    * @param position   of the BranchNode children where the child should be inserted.
    * @param child      to be inserted as a child of the new BranchNode (already hashed if necessary).
    * @param terminator to be associated with the new BranchNode.
    * @return a new BranchNode.
    */
  def withSingleChild(position: Byte, child: Either[Array[Byte], Node], terminator: Option[Array[Byte]]): BranchNode = {
    val children = Array.fill[Option[Either[Array[Byte], Node]]](Node.ListSize - 1)(None)
    children(position) = Some(child)
    BranchNode(children, terminator)
  }
}

final case class BranchNode(children: Array[Option[Either[Array[Byte], Node]]], terminator: Option[Array[Byte]]) extends Node {

  /**
    * This function creates a new BranchNode by updating one of the children of the self node.
    *
    * @param position of the BranchNode children where the child should be inserted.
    * @param childNode  to be inserted as a child of the new BranchNode (and hashed if necessary).
    * @return a new BranchNode.
    */
  def updateChild(position: Int, childNode: Node): BranchNode = {
    val childCapped = childNode.capped
    children(position) = Some(if (childCapped.length == 32) Left(childCapped) else Right(childNode))
    BranchNode(children, terminator)
  }

}