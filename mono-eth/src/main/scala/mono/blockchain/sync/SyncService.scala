package mono.blockchain.sync

import akka.actor.{ActorSystem, Props}

object SyncService {
  val name = "syncService"
  val managerName = "monoSingleton-" + name
  val managerPath = "/user/" + managerName
  val path = managerPath + "/" + name
  val proxyName = "monoSingletonProxy-" + name
  val proxyPath = "/user/" + proxyName


  def props() = Props(classOf[SyncService]).withDispatcher("mono-sync-pinned-dispatcher")
  def proxy(system: ActorSystem) = system.actorSelection(proxyPath)

}
class SyncService {

}
