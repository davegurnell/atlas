package atlas

import minitest._
import syntax._
import unindent._

object ExprEvalSuite extends SimpleTestSuite {
  test("constant") {
    assertSuccess(
      expr"true",
      Env.empty,
      true
    )
  }

  test("infix") {
    assertSuccess(
      expr"1 + 2 + 3",
      Env.empty,
      6
    )
  }

  test("variable reference") {
    assertSuccess(
      expr"foo",
      Env.empty.set("foo", true),
      true
    )
  }

  test("variable not in env") {
    assertFailure(
      expr"foo",
      Env.empty,
      Eval.Error("Not in scope: foo")
    )
  }

  test("function application") {
    val code = expr"""
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
    val code = expr"""
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

  def assertSuccess[A](expr: Ast.Expr, env: Env, expected: A)(implicit enc: ValueEncoder[A]): Unit =
    assertEquals(Eval(expr, env), Right(enc(expected)))

  def assertFailure(expr: Ast.Expr, env: Env, expected: Eval.Error): Unit =
    assertEquals(Eval(expr, env), Left(expected))
}
