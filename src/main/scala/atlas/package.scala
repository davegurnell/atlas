import cats.data.StateT

package object atlas {
  type EvalStep[F[_], A] = StateT[F, Env[F], A]
}
