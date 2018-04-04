package atlas

import atlas.syntax._
import cats.Eval
import cats.MonadError
import cats.data.EitherT
import cats.implicits._
import minitest._
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object SyncInterpreterSuite extends SimpleInterpreterSuite(SyncInterpreter) {
  def toEither[A](either: EitherT[Eval, RuntimeError, A]): Either[RuntimeError, A] =
    either.value.value
}

object AsyncInterpreterSuite extends SimpleInterpreterSuite(new AsyncInterpreter) {
  def toEither[A](eitherT: EitherT[Future, RuntimeError, A]): Either[RuntimeError, A] =
    Await.result(eitherT.value, 1.second)
}

abstract class SimpleInterpreterSuite[F[_]](interpreter: Interpreter[F])(implicit monad: MonadError[F, RuntimeError]) extends SimpleTestSuite {
  import interpreter.{createEnv, basicEnv, native}

  test("constant") {
    assertSuccess(
      expr"true",
      createEnv,
      true
    )
  }

  test("infix") {
    assertSuccess(
      expr"1 + 2 + 3",
      createEnv,
      6
    )
  }

  test("variable reference") {
    assertSuccess(
      expr"foo",
      createEnv.set("foo", true),
      true
    )
  }

  test("variable not in env") {
    assertFailure(
      expr"foo",
      createEnv,
      RuntimeError("Not in scope: foo")
    )
  }

  test("function application") {
    val code = expr"""
      add(mul(a, b), mul(4, 5))
      """

    val env = createEnv
     .set("add", native((a: Int, b: Int) => a + b))
     .set("mul", native((a: Int, b: Int) => a * b))
     .set("a", 2)
     .set("b", 3)

    val expected = 26

    assertSuccess(code, env, expected)
  }

  test("lexical scoping") {
    val code = expr"""
      do
        let a = 123
        let bound = () -> a
        do
          let a = 321
          bound()
        end
      end
      """

    val env = createEnv

    val expected = 123

    assertSuccess(code, env, expected)
  }

  test("object literals") {
    import atlas.syntax._

    val code = expr"""
      {
        foo: 1 + 1,
        bar: 'a' + 'b',
        baz: [ 1 + 2, 3 + 4]
      }
      """

    val env = createEnv

    val expected = ObjVal(List(
      "foo" -> 2.toAtlas[F],
      "bar" -> "ab".toAtlas[F],
      "baz" -> List(3, 7).toAtlas[F]
    ))

    assertSuccess(code, env, expected)
  }

  def assertSuccess[A](expr: Expr, env: Env[F], expected: A)(implicit enc: ValueEncoder[F, A]): Unit =
    assertEquals(toEither(interpreter(expr, env)), Right(enc(expected)))

  def assertFailure(expr: Expr, env: Env[F], expected: RuntimeError): Unit =
    assertEquals(toEither(interpreter(expr, env)), Left(expected))

  def toEither[A](value: F[A]): Either[RuntimeError, A]
}
