package atlas

final case class Scope(var bindings: Map[String, Value]) {
  def get(id: String): Option[Value] =
    bindings.collectFirst { case (`id`, value) => value }

  def set[A](id: String, value: Value): Scope =
    Scope(bindings + ((id, value)))

  def destructiveSet(id: String, value: Value): Unit =
    bindings = bindings + ((id, value))

  def destructiveSetAll(bindings: Seq[(String, Value)]): Unit =
    bindings.foreach { case (id, value) => destructiveSet(id, value) }
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

  def set(id: String, value: Value): Env =
    Env(scopes.head.set(id, value) :: scopes.tail)

  def set[A](id: String, value: A)(implicit enc: ValueEncoder[A]): Env =
    set(id, enc(value))

  def push: Env =
    Env(Scope.create :: scopes)

  def pop: Env =
    Env(scopes.tail)
}

object Env {
  def create: Env =
    create(Scope.create)

  def create(scope: Scope): Env =
    Env(List(scope))

  def basic: Env =
    Env.create
      .set("map", NativeFunc((func: Value => Value, list: List[Value]) => list.map(func)))
      .set("flatMap", NativeFunc((func: Value => List[Value], list: List[Value]) => list.flatMap(func)))
      .set("filter", NativeFunc((func: Value => Boolean, list: List[Value]) => list.filter(func)))
      .set("flatten", NativeFunc((list: List[List[Value]]) => list.flatten))
}
