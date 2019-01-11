package atlas

import atlas.syntax._
import cats.{Eval, MonadError}
import cats.data.EitherT
import cats.implicits._
import minitest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object SyncInterpreterExprSuite extends InterpreterExprSuite[EitherT[Eval, RuntimeError, ?]] {
  def toEither[A](either: EitherT[Eval, RuntimeError, A]): Either[RuntimeError, A] =
    either.value.value
}

object AsyncInterpreterExprSuite extends InterpreterExprSuite[EitherT[Future, RuntimeError, ?]] {
  def toEither[A](eitherT: EitherT[Future, RuntimeError, A]): Either[RuntimeError, A] =
    Await.result(eitherT.value, 1.second)
}

abstract class InterpreterExprSuite[F[_]](implicit monad: MonadError[F, RuntimeError]) extends InterpreterSuite[F] {
  test("literal") {
    assertSuccess(
      expr"true",
      true
    )
  }

  test("infix") {
    assertSuccess(
      expr"1 + 2 + 3",
      6
    )
  }

  test("variable reference") {
    assertSuccess(
      expr"foo",
      true,
      Env.create[F].set("foo", true),
    )
  }

  test("variable not in env") {
    assertFailure(
      expr"foo",
      RuntimeError("Not in scope: foo")
    )
  }

  test("function application") {
    val code = expr"""
      add(mul(a, b), mul(4, 5))
      """

    val env = Env.create[F]
     .set("add", Native((a: Int, b: Int) => a + b))
     .set("mul", Native((a: Int, b: Int) => a * b))
     .set("a", 2)
     .set("b", 3)

    val expected = 26

    assertSuccess(code, expected, env)
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

    val expected = 123

    assertSuccess(code, expected)
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

    val expected = ObjVal(List(
      "foo" -> 2.toAtlas[F],
      "bar" -> "ab".toAtlas[F],
      "baz" -> List(3, 7).toAtlas[F]
    ))

    assertSuccess(code, expected)
  }
}
