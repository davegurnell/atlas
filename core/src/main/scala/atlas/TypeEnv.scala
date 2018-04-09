package atlas

object TypeEnv {
  def create: TypeEnv =
    ScopeChain.create
}
