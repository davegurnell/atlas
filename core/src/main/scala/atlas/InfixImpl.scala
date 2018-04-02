package atlas

import cats.MonadError
import cats.syntax.all._

trait InfixImpl[F[_]] {
  self: Interpreter[F] =>

  private val add: Native[F] =
    native((a: Int, b: Int) => a + b) orElse
    native((a: Double, b: Double) => a + b) orElse
    native((a: String, b: String) => a + b)

  private val sub: Native[F] =
    native((a: Int, b: Int) => a - b) orElse
    native((a: Double, b: Double) => a - b)

  private val mul: Native[F] =
    native((a: Int, b: Int) => a * b) orElse
    native((a: Double, b: Double) => a * b)

  private val div: Native[F] =
    native((a: Int, b: Int) => 1.0 * a / b) orElse
    native((a: Double, b: Double) => a / b)

  private val and: Native[F] =
    native((a: Boolean, b: Boolean) => a && b)

  private val or: Native[F] =
    native((a: Boolean, b: Boolean) => a || b)

  private val eq: Native[F] =
    native((a: Value[F], b: Value[F]) => a == b)

  private val ne: Native[F] =
    native((a: Value[F], b: Value[F]) => a != b)

  private val gt: Native[F] =
    native((a: Int, b: Int) => a > b) orElse
    native((a: Double, b: Double) => a > b) orElse
    native((a: String, b: String) => a > b) orElse
    native((a: Boolean, b: Boolean) => a > b)

  private val lt: Native[F] =
    native((a: Int, b: Int) => a < b) orElse
    native((a: Double, b: Double) => a < b) orElse
    native((a: String, b: String) => a < b) orElse
    native((a: Boolean, b: Boolean) => a < b)

  private val gte: Native[F] =
    native((a: Int, b: Int) => a >= b) orElse
    native((a: Double, b: Double) => a >= b) orElse
    native((a: String, b: String) => a >= b) orElse
    native((a: Boolean, b: Boolean) => a >= b)

  private val lte: Native[F] =
    native((a: Int, b: Int) => a <= b) orElse
    native((a: Double, b: Double) => a <= b) orElse
    native((a: String, b: String) => a <= b) orElse
    native((a: Boolean, b: Boolean) => a <= b)

  private val pos: Native[F] =
    native((a: Int) => a) orElse
    native((a: Double) => a)

  private val neg: Native[F] =
    native((a: Int) => -a) orElse
    native((a: Double) => -a)

  private val not: Native[F] =
    native((a: Boolean) => !a)

  def infixImpl(op: InfixOp): Native[F] =
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
