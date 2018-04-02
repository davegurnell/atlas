package atlas

final case class Scope[K, V](var bindings: Map[K, V]) {
  def get(id: K): Option[V] =
    bindings.collectFirst { case (`id`, value) => value }

  def set(id: K, value: V): Scope[K, V] =
    Scope(bindings + ((id, value)))

  def destructiveSet(id: K, value: V): Unit =
    bindings = bindings + ((id, value))

  def destructiveSetAll(bindings: Seq[(K, V)]): Unit =
    bindings.foreach { case (id, value) => destructiveSet(id, value) }
}

final case class ScopeChain[K, V](scopes: List[Scope[K, V]]) {
  def get(id: K): Option[V] = {
    def loop(scopes: List[Scope[K, V]]): Option[V] =
      scopes match {
        case head :: tail => head.get(id).orElse(loop(tail))
        case Nil          => None
      }
    loop(scopes)
  }

  def set(id: K, value: V): ScopeChain[K, V] =
    ScopeChain(scopes.head.set(id, value) :: scopes.tail)

  def destructiveSet(id: K, value: V): Unit =
    scopes.head.destructiveSet(id, value)

  def destructiveSetAll(bindings: Seq[(K, V)]): Unit =
    scopes.head.destructiveSetAll(bindings)

  def push: ScopeChain[K, V] =
    ScopeChain(Scope.create[K, V] :: scopes)

  def pop: ScopeChain[K, V] =
    ScopeChain(scopes.tail)
}

object Scope {
  def create[K, V]: Scope[K, V] =
    Scope(Map.empty[K, V])
}

object ScopeChain {
  def create[K, V]: ScopeChain[K, V] =
    ScopeChain(List(Scope.create))
}
