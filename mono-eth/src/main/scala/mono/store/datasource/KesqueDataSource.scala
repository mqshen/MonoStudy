package mono.store.datasource

import akka.actor.ActorSystem
import kesque.{HashKeyValueTable, TKeyVal, TVal}
import mono.Hash
import mono.util.{Clock, SimpleMap}

object KesqueDataSource {
  val account = "account"
  val storage = "storage"
  val header = "header"
  val body = "body"
  val td = "td" // total difficulty
  val receipts = "receipts"
}

final class KesqueDataSource(val table: HashKeyValueTable, val topic: String)(implicit system: ActorSystem) extends SimpleMap[Hash, TVal, KesqueDataSource] {
  private var _currWritingBlockNumber: Long = _

  val clock = new Clock()
  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Set[Hash], toUpsert: Map[Hash, TVal]) = {
    table.write(toUpsert.map { case (key, value) =>
      println(s"source write key:$key")
      TKeyVal(key.bytes, value.value, _currWritingBlockNumber)
    }, topic)
    this
  }

  def setWritingBlockNumber(writingBlockNumber: Long) {
    this._currWritingBlockNumber = writingBlockNumber
  }

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  override def get(key: Hash): Option[TVal] = {
    println(s"source get key:$key")
    val start = System.currentTimeMillis
    val value = table.read(key.bytes, topic)
    clock.elapse(System.currentTimeMillis - start)
    value
  }
}
