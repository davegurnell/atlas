import cats.data.StateT

package object atlas {
  type Env     = ScopeChain[String, Value]
  type TypeEnv = ScopeChain[String, Type]
}
