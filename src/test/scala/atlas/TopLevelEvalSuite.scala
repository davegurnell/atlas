package atlas

import minitest._
import unindent._

object TopLevelEvalSuite extends SimpleTestSuite {
  test("recursive bindings") {
    this.ignore()

    val code = i"""
      let even = n -> if n == 0 then true else odd(n - 1)
      let odd  = n -> if n == 0 then false else even(n - 1)
      let result = even(10)
      """

    val env = Env.empty

    val expected = true

    assertSuccess(code, env, expected)
  }

  def assertSuccess[A](code: String, env: Env, expected: A)(implicit enc: ValueEncoder[A]): Unit = {
    Parser.topLevel(code) match {
      case Right(topLevel) =>
        assertEquals(Eval(topLevel, env, Ast.Ref("result")), Right(expected))
      case Left(error) =>
        fail("Could not parse expression: " + error)
    }
  }

  def assertFailure(code: String, env: Env, expected: Eval.Error): Unit = {
    Parser.topLevel(code) match {
      case Right(topLevel) =>
        assertEquals(Eval(topLevel, env, Ast.Ref("result")), Left(expected))
      case Left(error) =>
        fail("Could not parse expression: " + error)
    }
  }
}
