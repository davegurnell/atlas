package atlas

import cats.MonadError

final case class Env(chain: ScopeChain[String, Value]) {
  def get(id: String): Option[Value] =
    chain.get(id)

  def set[A](id: String, value: A)(implicit enc: ValueEncoder[A]): Env =
    Env(chain.set(id, enc(value)))

  def destructiveSet[A](id: String, value: A)(implicit enc: ValueEncoder[A]): Unit =
    chain.destructiveSet(id, enc(value))

  def destructiveSetAll[A](bindings: Seq[(String, A)])(implicit enc: ValueEncoder[A]): Unit =
    chain.destructiveSetAll(bindings.map { case (n, a) => (n, enc(a)) })

  def push: Env =
    Env(chain.push)

  def pop: Env =
    Env(chain.pop)
}

object Env {
  def create: Env =
    Env(ScopeChain.create)

  def basic: Env =
    Env.create
      // .set("map",     (func: Value => Value, list: List[Value]) => list.map(func))
      // .set("flatMap", (func: Value => List[Value], list: List[Value]) => list.flatMap(func))
      // .set("filter",  (func: Value => Boolean, list: List[Value]) => list.filter(func))
      // .set("flatten", (list: List[List[Value]]) => list.flatten)
}
