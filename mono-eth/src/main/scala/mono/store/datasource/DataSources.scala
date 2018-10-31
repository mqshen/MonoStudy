package mono.store.datasource

trait DataSources {

  val appStateDataSource: DataSource
  val transactionMappingDataSource: DataSource

}
