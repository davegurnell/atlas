package atlas

import cats.MonadError
import cats.syntax.all._

final case class NativeFunc[F[_]](func: List[Value] => F[Value]) {
  def apply(args: List[Value]): F[Value] =
    func(args)

  def orElse(that: NativeFunc[F])(implicit monadError: MonadError[F, RuntimeError]): NativeFunc[F] =
    NativeFunc(args => this(args).recoverWith { case err => that(args) })
}

abstract class NativeImpl[F[_]](implicit monadError: MonadError[F, RuntimeError]) extends NativeBoilerplate {
  def add: NativeFunc[F] =
    wrap((a: Int, b: Int) => a + b) orElse
    wrap((a: Double, b: Double) => a + b) orElse
    wrap((a: String, b: String) => a + b)

  def sub: NativeFunc[F] =
    wrap((a: Int, b: Int) => a - b) orElse
    wrap((a: Double, b: Double) => a - b)

  def mul: NativeFunc[F] =
    wrap((a: Int, b: Int) => a * b) orElse
    wrap((a: Double, b: Double) => a * b)

  def div: NativeFunc[F] =
    wrap((a: Int, b: Int) => 1.0 * a / b) orElse
    wrap((a: Double, b: Double) => a / b)

  def and: NativeFunc[F] =
    wrap((a: Boolean, b: Boolean) => a && b)

  def or: NativeFunc[F] =
    wrap((a: Boolean, b: Boolean) => a || b)

  def eq: NativeFunc[F] =
    wrap((a: Value, b: Value) => a == b)

  def ne: NativeFunc[F] =
    wrap((a: Value, b: Value) => a != b)

  def gt: NativeFunc[F] =
    wrap((a: Int, b: Int) => a > b) orElse
    wrap((a: Double, b: Double) => a > b) orElse
    wrap((a: String, b: String) => a > b) orElse
    wrap((a: Boolean, b: Boolean) => a > b)

  def lt: NativeFunc[F] =
    wrap((a: Int, b: Int) => a < b) orElse
    wrap((a: Double, b: Double) => a < b) orElse
    wrap((a: String, b: String) => a < b) orElse
    wrap((a: Boolean, b: Boolean) => a < b)

  def gte: NativeFunc[F] =
    wrap((a: Int, b: Int) => a >= b) orElse
    wrap((a: Double, b: Double) => a >= b) orElse
    wrap((a: String, b: String) => a >= b) orElse
    wrap((a: Boolean, b: Boolean) => a >= b)

  def lte: NativeFunc[F] =
    wrap((a: Int, b: Int) => a <= b) orElse
    wrap((a: Double, b: Double) => a <= b) orElse
    wrap((a: String, b: String) => a <= b) orElse
    wrap((a: Boolean, b: Boolean) => a <= b)

  def pos: NativeFunc[F] =
    wrap((a: Int) => a) orElse
    wrap((a: Double) => a)

  def neg: NativeFunc[F] =
    wrap((a: Int) => -a) orElse
    wrap((a: Double) => -a)

  def not: NativeFunc[F] =
    wrap((a: Boolean) => !a)

  def nativeImpl(op: NativeOp): F[NativeFunc[F]] =
    op match {
      case InfixOp.Add  => add.pure
      case InfixOp.Sub  => sub.pure
      case InfixOp.Mul  => mul.pure
      case InfixOp.Div  => div.pure
      case InfixOp.And  => and.pure
      case InfixOp.Or   => or.pure
      case InfixOp.Eq   => eq.pure
      case InfixOp.Ne   => ne.pure
      case InfixOp.Gt   => gt.pure
      case InfixOp.Lt   => lt.pure
      case InfixOp.Gte  => gte.pure
      case InfixOp.Lte  => lte.pure
      case PrefixOp.Pos => pos.pure
      case PrefixOp.Neg => neg.pure
      case PrefixOp.Not => not.pure
      case other        => RuntimeError(s"Native implementation not found: $op").raiseError
    }
}
