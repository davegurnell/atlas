package atlas

import cats.MonadError
import minitest._
import minitest.api.SourceLocation

abstract class InterpreterSuite[F[_]](interpreter: Interpreter[F])(implicit monad: MonadError[F, RuntimeError]) extends SimpleTestSuite {
  def assertSuccess[A](expr: ExprStx, expected: A, env: Env[F] = interpreter.env, limits: Limits = Limits.create)(implicit enc: ValueEncoder[F, A], loc: SourceLocation): Unit =
    assertEquals(toEither(interpreter.eval(expr.desugar, env, limits)), Right(enc(expected)))

  def assertFailure(expr: ExprStx, expected: RuntimeError, env: Env[F] = interpreter.env, limits: Limits = Limits.create)(implicit loc: SourceLocation): Unit =
    assertEquals(toEither(interpreter.eval(expr.desugar, env, limits)), Left(expected))

  def toEither[A](value: F[A]): Either[RuntimeError, A]
}
