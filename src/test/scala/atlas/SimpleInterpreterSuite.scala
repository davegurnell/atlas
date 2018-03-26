package atlas

import atlas.syntax._
import cats.implicits._
import minitest._

object SimpleInterpreterSuite extends SimpleTestSuite {
  import SyncInterpreter.{F, createEnv, basicEnv}

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
     // .set("add", (a: Int, b: Int) => a + b)
     // .set("mul", (a: Int, b: Int) => a * b)
     // .set("a", 2)
     // .set("b", 3)

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
    import io.circe._
    import io.circe.syntax._

    val code = expr"""
      {
        foo: 1 + 1,
        bar: 'a' + 'b',
        baz: [ 1 + 2, 3 + 4]
      }
      """

    val env = createEnv

    val expected = Json.obj(
      "foo" -> 2.asJson,
      "bar" -> "ab".asJson,
      "baz" -> List(3, 7).asJson
    )

    assertSuccess(code, env, expected)
  }

  def assertSuccess[A](expr: Expr, env: Env[F], expected: A)(implicit enc: ValueEncoder[F, A]): Unit =
    assertEquals(SyncInterpreter(expr, env), Right(enc(expected)))

  def assertFailure(expr: Expr, env: Env[F], expected: RuntimeError): Unit =
    assertEquals(SyncInterpreter(expr, env), Left(expected))
}
