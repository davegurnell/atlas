package atlas

object Env {
  def create[F[_]]: Env =
    ScopeChain.create
}
