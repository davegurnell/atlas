package atlas

object Env {
  def create[F[_]]: Env[F] =
    ScopeChain.create
}
