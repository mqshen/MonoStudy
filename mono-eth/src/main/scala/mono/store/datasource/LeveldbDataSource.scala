package mono.store.datasource

import java.io.File

import mono.util.BytesUtil
import org.iq80.leveldb.{DB, Options, WriteOptions}
import org.iq80.leveldb.impl.Iq80DBFactory


trait LeveldbConfig {
  val path: String
  val createIfMissing: Boolean
  val paranoidChecks: Boolean
  val verifyChecksums: Boolean
  val cacheSize: Long
}

object LeveldbDataSource {

  private def createDB(levelDbConfig: LeveldbConfig): DB = {
    import levelDbConfig._

    val options = new Options()
      .compressionType(org.iq80.leveldb.CompressionType.SNAPPY)
      .createIfMissing(createIfMissing)
      .paranoidChecks(paranoidChecks) // raise an error as soon as it detects an internal corruption
      .verifyChecksums(verifyChecksums) // force checksum verification of all data that is read from the file system on behalf of a particular read
      .cacheSize(cacheSize)

    Iq80DBFactory.factory.open(new File(path), options)
  }

  def apply(levelDbConfig: LeveldbConfig): LeveldbDataSource = {
    new LeveldbDataSource(createDB(levelDbConfig), levelDbConfig)
  }
}

final class LeveldbDataSource(
  private var db:            DB,
  private val levelDbConfig: LeveldbConfig
) extends DataSource {
  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key) = {
    val dbKey = BytesUtil.concat(namespace, key)
    val start = System.currentTimeMillis
    val value = db.get(dbKey)
    clock.elapse(System.currentTimeMillis - start)
    Option(value)
  }

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    *
    * Note: toRemove and toUpsert should have been processed orderly properly, and so as
    * then, the keys in toRemove are not contained in toUpsert and vice versa. This
    * makes the order of toRemove and toUpsert free
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param toRemove  which includes all the keys to be removed from the DataSource.
    * @param toUpsert  which includes all the (key-value) pairs to be inserted into the DataSource.
    *                  If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(namespace: Namespace, toRemove: Iterable[Key], toUpsert: Iterable[(Key, Value)]) = {
    val batch = db.createWriteBatch()
    toRemove.foreach { key => batch.delete(BytesUtil.concat(namespace, key)) }
    toUpsert.foreach { case (key, value) => batch.put(BytesUtil.concat(namespace, key), value) }
    db.write(batch, new WriteOptions())
    this
  }
}
