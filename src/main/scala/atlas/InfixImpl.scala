package atlas

import cats.MonadError
import cats.syntax.all._

object InfixImpl extends NativeBoilerplate {
  def add[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => a + b) orElse
    Value.native((a: Double, b: Double) => a + b) orElse
    Value.native((a: String, b: String) => a + b)

  def sub[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => a - b) orElse
    Value.native((a: Double, b: Double) => a - b)

  def mul[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => a * b) orElse
    Value.native((a: Double, b: Double) => a * b)

  def div[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => 1.0 * a / b) orElse
    Value.native((a: Double, b: Double) => a / b)

  def and[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Boolean, b: Boolean) => a && b)

  def or[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Boolean, b: Boolean) => a || b)

  def eq[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Value[F], b: Value[F]) => a == b)

  def ne[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Value[F], b: Value[F]) => a != b)

  def gt[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => a > b) orElse
    Value.native((a: Double, b: Double) => a > b) orElse
    Value.native((a: String, b: String) => a > b) orElse
    Value.native((a: Boolean, b: Boolean) => a > b)

  def lt[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => a < b) orElse
    Value.native((a: Double, b: Double) => a < b) orElse
    Value.native((a: String, b: String) => a < b) orElse
    Value.native((a: Boolean, b: Boolean) => a < b)

  def gte[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => a >= b) orElse
    Value.native((a: Double, b: Double) => a >= b) orElse
    Value.native((a: String, b: String) => a >= b) orElse
    Value.native((a: Boolean, b: Boolean) => a >= b)

  def lte[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int, b: Int) => a <= b) orElse
    Value.native((a: Double, b: Double) => a <= b) orElse
    Value.native((a: String, b: String) => a <= b) orElse
    Value.native((a: Boolean, b: Boolean) => a <= b)

  def pos[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int) => a) orElse
    Value.native((a: Double) => a)

  def neg[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Int) => -a) orElse
    Value.native((a: Double) => -a)

  def not[F[_]](implicit monad: MonadError[F, RuntimeError]): Native[F] =
    Value.native((a: Boolean) => !a)

  def apply[F[_]](op: InfixOp)(implicit monad: MonadError[F, RuntimeError]): Native[F] =
    op match {
      case InfixOp.Add  => add
      case InfixOp.Sub  => sub
      case InfixOp.Mul  => mul
      case InfixOp.Div  => div
      case InfixOp.And  => and
      case InfixOp.Or   => or
      case InfixOp.Eq   => eq
      case InfixOp.Ne   => ne
      case InfixOp.Gt   => gt
      case InfixOp.Lt   => lt
      case InfixOp.Gte  => gte
      case InfixOp.Lte  => lte
    }
}
