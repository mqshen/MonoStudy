package mono.store.datasource

import mono.util.Config

trait SharedLeveldbDataSources extends DataSources {
  val dataSource = LeveldbDataSource(Config.Db.Leveldb)


  val appStateDataSource = dataSource

}
