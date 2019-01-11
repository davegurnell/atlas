package atlas

import cats.MonadError
import minitest._

abstract class InterpreterSuite[F[_]](interpreter: Interpreter[F])(implicit monad: MonadError[F, RuntimeError]) extends SimpleTestSuite {
  def assertSuccess[A](expr: Expr, expected: A, env: Env = Env.create[F], limits: Limits = Limits.create)(implicit enc: ValueEncoder[F, A]): Unit =
    assertEquals(toEither(interpreter.eval(expr, env, limits)), Right(enc(expected)))

  def assertFailure(expr: Expr, expected: RuntimeError, env: Env = Env.create[F], limits: Limits = Limits.create): Unit =
    assertEquals(toEither(interpreter.eval(expr, env, limits)), Left(expected))

  def toEither[A](value: F[A]): Either[RuntimeError, A]
}
