package mono.vm

trait WorldState[W <: WorldState[W, S], S <: Storage[S]] { self: W =>

}
