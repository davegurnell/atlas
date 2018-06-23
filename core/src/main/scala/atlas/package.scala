import cats.data.StateT

package object atlas {
  type Env[F[_]] = ScopeChain[String, Value[F]]
  type EvalStep[F[_], A] = StateT[F, (Env[F], Limits), A]

  type DesugarEnv = ScopeChain[String, Expr]
  type DesugarStep[F[_], A] = StateT[F, Int, A]

  type TypeEnv = ScopeChain[String, Type]
  type TypeStep[F[_], A] = StateT[F, (Int, TypeEnv), A]
}
