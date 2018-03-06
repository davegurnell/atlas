package atlas

final case class Scope(var bindings: Map[String, Value]) {
  def get(id: String): Option[Value] =
    bindings.collectFirst { case (`id`, value) => value }

  def set(id: String, value: Value): Unit =
    bindings = bindings + ((id, value))
}

object Scope {
  def create: Scope =
    Scope(Map())
}

final case class Env(scopes: List[Scope]) {
  def get(id: String): Option[Value] = {
    def loop(scopes: List[Scope]): Option[Value] =
      scopes match {
        case head :: tail => head.get(id).orElse(loop(tail))
        case Nil          => None
      }
    loop(scopes)
  }

  def set(id: String, value: Value): Env = {
    scopes.head.set(id, value)
    this
  }

  def set[A](id: String, value: A)(implicit enc: ValueEncoder[A]): Env = {
    scopes.head.set(id, enc(value))
    this
  }

  def setAll(bindings: Seq[(String, Value)]): Env = {
    bindings.foreach { case (id, value) => set(id, value) }
    this
  }

  def push: Env =
    Env(Scope.create :: scopes)

  def pop: Env =
    Env(scopes.tail)
}

object Env {
  def create: Env =
    Env(List(Scope(Map())))
}
