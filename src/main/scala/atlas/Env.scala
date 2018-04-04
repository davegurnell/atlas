package atlas

final case class Env[F[_]](chain: ScopeChain[String, Value[F]]) {
  def get(id: String): Option[Value[F]] =
    chain.get(id)

  def set[A](id: String, value: A)(implicit enc: ValueEncoder[F, A]): Env[F] =
    Env(chain.set(id, enc(value)))

  def destructiveSet[A](id: String, value: A)(implicit enc: ValueEncoder[F, A]): Unit =
    chain.destructiveSet(id, enc(value))

  def destructiveSetAll[A](bindings: Seq[(String, A)])(implicit enc: ValueEncoder[F, A]): Unit =
    chain.destructiveSetAll(bindings.map { case (n, a) => (n, enc(a)) })

  def push: Env[F] =
    Env(chain.push)

  def pop: Env[F] =
    Env(chain.pop)
}

object Env {
  def create[F[_]]: Env[F] =
    Env(ScopeChain.create)
}
