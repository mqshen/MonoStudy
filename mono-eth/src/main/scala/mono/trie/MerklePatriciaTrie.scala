package mono.trie

import java.util.Arrays

import akka.util.ByteString
import mono._
import mono.store.trienode.NodeKeyValueStorage
import mono.util.{BytesUtil, SimpleMap}

import scala.annotation.tailrec

object MerklePatriciaTrie {
  private case class NodeInsertResult(
    newNode: Node,
    changes: Vector[Changed[Node]] = Vector()
  )

  private def matchingLength(a: Array[Byte], b: Array[Byte]): Int = {
    var i = 0
    while (i < a.length && i < b.length && a(i) == b(i)) {
      i += 1
    }
    i
  }

  final case class MPTNodeMissingException(message: String, hash: Hash, table: String) extends RuntimeException(message)

  def apply[K, V](source: NodeKeyValueStorage)(implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V]): MerklePatriciaTrie[K, V] =
    new MerklePatriciaTrie[K, V](None, source, Map())(kSerializer, vSerializer)

  def apply[K, V](rootHash: Array[Byte], source: NodeKeyValueStorage)(implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V]): MerklePatriciaTrie[K, V] = {
    if (Arrays.equals(EmptyTrieHash, rootHash)) {
      new MerklePatriciaTrie[K, V](None, source, Map())(kSerializer, vSerializer)
    } else {
      new MerklePatriciaTrie[K, V](Some(rootHash), source, Map())(kSerializer, vSerializer)
    }
  }

}

final class MerklePatriciaTrie[K, V] private (
  rootHashOpt:          Option[Array[Byte]],
  nodeStorage:          NodeKeyValueStorage,
  private var nodeLogs: Map[Hash, Log[Array[Byte]]]
)(implicit kSerializer: ByteArrayEncoder[K], vSerializer: ByteArraySerializable[V]) extends SimpleMap[K, V, MerklePatriciaTrie[K, V]] {
  import MerklePatriciaTrie._

  // The root hash will be already here via a series of put/remove operations
  lazy val rootHash: Array[Byte] = rootHashOpt.getOrElse(EmptyTrieHash)
  /**
    * Obtains the value asociated with the key passed, if there exists one.
    * The trie should keep in same state without side effect
    *
    * @param key
    * @return Option object with value if there exists one.
    * @throws mono.trie.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  def get(key: K): Option[V] = {
    rootHashOpt flatMap { rootId =>
      val rootNode = getNode(rootId)
      val keyNibbles = HexPrefix.bytesToNibbles(kSerializer.toBytes(key))
      get(rootNode, keyNibbles).map(vSerializer.fromBytes)
    }
  }

  @tailrec
  private def get(node: Node, searchKey: Array[Byte]): Option[Array[Byte]] = node match {
    case LeafNode(key, value) =>
      if (Arrays.equals(key, searchKey)) {
        Some(value)
      } else {
        None
      }

    case extNode @ ExtensionNode(sharedKey, _) =>
      if (searchKey.length >= sharedKey.length) {
        val (commonKey, remainingKey) = BytesUtil.split(searchKey, sharedKey.length)
        if (Arrays.equals(sharedKey, commonKey)) {
          val nextNode = getNextNode(extNode)
          get(nextNode, remainingKey)
        } else {
          None
        }
      } else {
        None
      }

    case branch @ BranchNode(_, terminator) =>
      if (searchKey.length == 0) {
        terminator
      } else {
        getChild(branch, searchKey(0)) match {
          case Some(child) =>
            get(child, BytesUtil.tail(searchKey))
          case None =>
            None
        }
      }
  }
  /**
    * This function inserts a (key-value) pair into the trie. If the key is already asociated with another value it is updated.
    *
    * @param key
    * @param value
    * @return New trie with the (key-value) pair inserted.
    * @throws mono.trie.MerklePatriciaTrie.MPTException if there is any inconsistency in how the trie is build.
    */
  override def put(key: K, value: V): MerklePatriciaTrie[K, V] = {
    val keyNibbles = HexPrefix.bytesToNibbles(kSerializer.toBytes(key))
    println(s"put node key:${toHexString(kSerializer.toBytes(key))}")

    rootHashOpt map { rootId =>
      val root = getNode(rootId)
      val NodeInsertResult(newRoot, changes) = put(root, keyNibbles, vSerializer.toBytes(value))
      new MerklePatriciaTrie(
        Some(newRoot.hash),
        nodeStorage,
        updateNodesToLogs(previousRootHash = ByteString(rootHash), newRoot = Some(newRoot), changes = changes)
      )(kSerializer, vSerializer)
    } getOrElse {
      val newRoot = LeafNode(keyNibbles, vSerializer.toBytes(value))
      new MerklePatriciaTrie(
        Some(newRoot.hash),
        nodeStorage,
        updateNodesToLogs(ByteString(rootHash), Some(newRoot), Vector(Updated(newRoot)))
      )(kSerializer, vSerializer)
    }

  }

  private def put(node: Node, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = node match {
    case leafNode: LeafNode           => putInLeafNode(leafNode, searchKey, value)
    case extensionNode: ExtensionNode => putInExtensionNode(extensionNode, searchKey, value)
    case branchNode: BranchNode       => putInBranchNode(branchNode, searchKey, value)
  }

  private def putInLeafNode(node: LeafNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val LeafNode(existingKey, storedValue) = node
    matchingLength(existingKey, searchKey) match {
      case ml if ml == existingKey.length && ml == searchKey.length =>
        // We are trying to insert a leaf node that has the same key as this one but different value so we need to
        // replace it
        val newLeafNode = LeafNode(existingKey, value)
        NodeInsertResult(
          newNode = newLeafNode,
          changes = Vector(Deleted(node), Updated(newLeafNode))
        )
      case 0 =>
        // There is no common prefix between the node which means that we need to replace this leaf node
        val (temporalBranchNode, maybeNewLeaf) = if (existingKey.length == 0) {
          // This node has no key so it should be stored as branch's value
          (BranchNode.withValueOnly(storedValue), None)
        } else {
          // The leaf should be put inside one of new branch nibbles
          val newLeafNode = LeafNode(BytesUtil.tail(existingKey), storedValue)
          (BranchNode.withSingleChild(existingKey(0), newLeafNode, None), Some(newLeafNode))
        }
        val NodeInsertResult(newBranchNode: BranchNode, changes) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          changes = (changes :+ Deleted(node)) ++ maybeNewLeaf.map(Updated(_))
        )
      case ml =>
        // Partially shared prefix, we replace the leaf with an extension and a branch node
        val (searchKeyPrefix, searchKeySuffix) = BytesUtil.split(searchKey, ml)
        val temporalNode = if (ml == existingKey.length) {
          BranchNode.withValueOnly(storedValue)
        } else {
          LeafNode(BytesUtil.drop(existingKey, ml), storedValue)
        }
        val NodeInsertResult(newBranchNode: BranchNode, changes) = put(temporalNode, searchKeySuffix, value)
        val newExtNode = ExtensionNode(searchKeyPrefix, newBranchNode)
        NodeInsertResult(
          newNode = newExtNode,
          changes = (changes :+ Deleted(node)) :+ Updated(newExtNode)
        )
    }
  }

  private def putInExtensionNode(extensionNode: ExtensionNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val ExtensionNode(sharedKey, next) = extensionNode
    matchingLength(sharedKey, searchKey) match {
      case 0 =>
        // There is no common prefix with the node which means we have to replace it for a branch node
        val sharedKeyHead = sharedKey(0)
        val (temporalBranchNode, maybeNewLeaf) = if (sharedKey.length == 1) {
          // Direct extension, we just replace the extension with a branch
          (BranchNode.withSingleChild(sharedKeyHead, next, None), None)
        } else {
          // The new branch node will have an extension that replaces current one
          val newExtNode = ExtensionNode(BytesUtil.tail(sharedKey), next)
          (BranchNode.withSingleChild(sharedKeyHead, newExtNode, None), Some(newExtNode))
        }
        val NodeInsertResult(newBranchNode: BranchNode, changes) = put(temporalBranchNode, searchKey, value)
        NodeInsertResult(
          newNode = newBranchNode,
          changes = (changes :+ Deleted(extensionNode)) ++ maybeNewLeaf.map(Updated(_))
        )
      case ml if ml == sharedKey.length =>
        // Current extension node's key is a prefix of the one being inserted, so we insert recursively on the extension's child
        val NodeInsertResult(newChild: BranchNode, changes) = put(getNextNode(extensionNode), BytesUtil.drop(searchKey, ml), value)
        val newExtNode = ExtensionNode(sharedKey, newChild)
        NodeInsertResult(
          newNode = newExtNode,
          changes = (changes :+ Deleted(extensionNode)) :+ Updated(newExtNode)
        )
      case ml =>
        // Partially shared prefix, we have to replace the node with an extension with the shared prefix
        val (sharedKeyPrefix, sharedKeySuffix) = BytesUtil.split(sharedKey, ml)
        val temporalExtensionNode = ExtensionNode(sharedKeySuffix, next)
        val NodeInsertResult(newBranchNode: BranchNode, changes) = put(temporalExtensionNode, BytesUtil.drop(searchKey, ml), value)
        val newExtNode = ExtensionNode(sharedKeyPrefix, newBranchNode)
        NodeInsertResult(
          newNode = newExtNode,
          changes = (changes :+ Deleted(extensionNode)) :+ Updated(newExtNode)
        )
    }
  }

  private def putInBranchNode(branchNode: BranchNode, searchKey: Array[Byte], value: Array[Byte]): NodeInsertResult = {
    val BranchNode(children, _) = branchNode
    if (searchKey.isEmpty) {
      // The key is empty, the branch node should now be a terminator node with the new value asociated with it
      val newBranchNode = BranchNode(children, Some(value))
      NodeInsertResult(
        newNode = newBranchNode,
        changes = Vector(Deleted(branchNode), Updated(newBranchNode))
      )
    } else {
      // Non empty key, we need to insert the value in the correct branch node's child
      val searchKeyHead = searchKey(0).toInt
      val searchKeyTail = BytesUtil.tail(searchKey)
      if (children(searchKeyHead).isDefined) {
        // The associated child is not empty, we recursively insert in that child
        val NodeInsertResult(changedChild, changes) = put(getChild(branchNode, searchKeyHead).get, searchKeyTail, value)
        val newBranchNode = branchNode.updateChild(searchKeyHead, changedChild)
        NodeInsertResult(
          newNode = newBranchNode,
          changes = (changes :+ Deleted(branchNode)) :+ Updated(newBranchNode)
        )
      } else {
        // The associated child is empty, we just replace it with a leaf
        val newLeafNode = LeafNode(searchKeyTail, value)
        val newBranchNode = branchNode.updateChild(searchKeyHead, newLeafNode)
        NodeInsertResult(
          newNode = newBranchNode,
          changes = Vector(Deleted(branchNode), Updated(newLeafNode), Updated(newBranchNode))
        )
      }
    }
  }

  private def updateNodesToLogs(
    previousRootHash: ByteString,
    newRoot:          Option[Node],
    changes:          Vector[Changed[Node]]
  ): Map[Hash, Log[Array[Byte]]] = {
    val rootCapped = newRoot.map(_.capped).getOrElse(Array.emptyByteArray)

    val orderlyDedeplucated = changes.foldLeft(Map[Hash, Changed[Node]]()) {
      case (acc, nodeToRemove @ Deleted(node)) => acc + (Hash(node.hash) -> nodeToRemove)
      case (acc, nodeToUpdate @ Updated(node)) => acc + (Hash(node.hash) -> nodeToUpdate)
    }

    val (toRemove, toUpdate) = orderlyDedeplucated.foldLeft((Map[Hash, Log[Array[Byte]]](), Map[Hash, Log[Array[Byte]]]())) {
      case ((toRemove, toUpdate), (hash, Deleted(node))) =>
        val nCapped = node.capped
        if (nCapped.length == 32 || hash == previousRootHash) {
          (toRemove + (hash -> Deleted(Array.emptyByteArray)), toUpdate)
        } else {
          (toRemove, toUpdate)
        }
      case ((toRemove, toUpdate), (hash, Updated(node))) =>
        val nCapped = node.capped
        if (nCapped.length == 32 || nCapped == rootCapped) {
          (toRemove, toUpdate + (hash -> Updated(node.encoded)))
        } else {
          (toRemove, toUpdate)
        }
    }

    nodeLogs ++ toRemove ++ toUpdate
  }

  private def getChild(branchNode: BranchNode, pos: Int): Option[Node] =
    branchNode.children(pos) map {
      case Right(node) => node
      case Left(hash)  => getNode(hash)
    }

  private def getNextNode(extensionNode: ExtensionNode): Node =
    extensionNode.next match {
      case Right(node) => node
      case Left(hash)  => getNode(hash)
    }

  // --- node storage related
  private def getNode(nodeId: Array[Byte]): Node = {
    val encoded = if (nodeId.length < 32) {
      nodeId
    } else {
      val id = Hash(nodeId)
      nodeLogs.get(id) match {
        case Some(Original(x)) => x
        case Some(Updated(x))  => x
        case None =>
          nodeStorage.get(id) match {
            case Some(x) =>
              nodeLogs += (id -> Original(x))
              x
            case None =>
              throw MPTNodeMissingException(s"Node not found ${mono.toHexString(nodeId)}, trie is inconsistent", id, nodeStorage.tableName)
          }
        case Some(Deleted(_)) =>
          throw MPTNodeMissingException(s"Node has been deleted ${mono.toHexString(nodeId)}, trie is inconsistent", id, nodeStorage.tableName)
      }

    }
    rlp.decode[Node](encoded)(Node.nodeDec)
  }

  /**
    * Compose trie by toRemove and toUpsert
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new trie after the removals and insertions were done.
    */
  override def update(toRemove: Set[K], toUpsert: Map[K, V]): MerklePatriciaTrie[K, V] = {
    throw new UnsupportedOperationException("Use put/remove")
  }

  def persist(): MerklePatriciaTrie[K, V] = {
    val changes = nodeLogs.collect {
      case (k, Deleted(_)) => k -> None
      case (k, Updated(v)) => k -> Some(v)
    }
    nodeStorage.update(changes)
    this
  }
}