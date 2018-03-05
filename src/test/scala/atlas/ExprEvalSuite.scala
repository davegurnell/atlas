package atlas

import minitest._
import unindent._

object ExprEvalSuite extends SimpleTestSuite {
  test("constant") {
    assertSuccess(
      "true",
      Env.empty,
      true
    )
  }

  test("infix") {
    assertSuccess(
      "1 + 2 + 3",
      Env.empty,
      6
    )
  }

  test("variable reference") {
    assertSuccess(
      "foo",
      Env.empty.set("foo", true),
      true
    )
  }

  test("variable not in env") {
    assertFailure(
      "foo",
      Env.empty,
      Eval.Error("Not in scope: foo")
    )
  }

  test("function application") {
    val code = i"""
      add(mul(a, b), mul(4, 5))
      """

    val env = Env.empty
     .set("add", (a: Int, b: Int) => a + b)
     .set("mul", (a: Int, b: Int) => a * b)
     .set("a", 2)
     .set("b", 3)

    val expected = 26

    assertSuccess(code, env, expected)
  }

  test("lexical scoping") {
    val code = i"""
      do
        let a = 1
        let bound = () -> a
        let a = 2
        bound()
      end
      """

    val env = Env.empty

    val expected = 1

    assertSuccess(code, env, expected)
  }

  def assertSuccess[A](code: String, env: Env, expected: A)(implicit enc: ValueEncoder[A]): Unit = {
    Parser.expr(code) match {
      case Right(expr) =>
        assertEquals(Eval(expr, env), Right(enc(expected)))
      case Left(error) =>
        fail("Could not parse expression: " + error)
    }
  }

  def assertFailure(code: String, env: Env, expected: Eval.Error): Unit = {
    Parser.expr(code) match {
      case Right(expr) =>
        assertEquals(Eval(expr, env), Left(expected))
      case Left(error) =>
        fail("Could not parse expression: " + error)
    }
  }
}
