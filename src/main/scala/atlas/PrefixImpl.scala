package atlas

import cats.MonadError

object PrefixImpl {
  import Value._

  def pos[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int) => a) orElse
    Value.native((a: Double) => a)

  def neg[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int) => -a) orElse
    Value.native((a: Double) => -a)

  def not[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Boolean) => !a)

  def apply[F[_]](op: PrefixOp)(implicit monad: MonadError[F, RuntimeError]): Native[F] =
    op match {
      case PrefixOp.Pos => pos
      case PrefixOp.Neg => neg
      case PrefixOp.Not => not
    }
}
