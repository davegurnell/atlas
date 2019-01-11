package atlas

import cats.MonadError
import cats.implicits._
import minitest._

abstract class InterpreterSuite[F[_]](implicit monad: MonadError[F, RuntimeError]) extends SimpleTestSuite {
  def assertSuccess[A](expr: Expr, expected: A, env: Env = Env.create)(implicit enc: ValueEncoder[A]): Unit =
    assertEquals(toEither(Interpreter.eval[F](expr, env)), Right(enc(expected)))

  def assertFailure(expr: Expr, expected: RuntimeError, env: Env = Env.create): Unit =
    assertEquals(toEither(Interpreter.eval[F](expr, env)), Left(expected))

  def toEither[A](value: F[A]): Either[RuntimeError, A]
}
