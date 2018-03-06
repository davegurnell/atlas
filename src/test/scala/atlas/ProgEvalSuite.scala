package atlas

import minitest._
import unindent._

object ProgEvalSuite extends SimpleTestSuite {
  test("recursive bindings") {
    val code = i"""
      let even = n -> if n == 0 then true else odd(n - 1)
      let odd  = n -> if n == 0 then false else even(n - 1)
      let result = even(10)
      """

    val env = Env.create

    val expected = true

    assertSuccess(code, env, expected)
  }

  def assertSuccess[A](code: String, env: Env, expected: A)(implicit dec: ValueDecoder[A]): Unit = {
    Parser.prog(code) match {
      case Right(topLevel) =>
        assertEquals(Eval(topLevel, env, Ast.Ref("result")).flatMap(dec.apply), Right(expected))
      case Left(error) =>
        fail("Could not parse expression: " + error)
    }
  }

  def assertFailure(code: String, env: Env, expected: Eval.Error): Unit = {
    Parser.prog(code) match {
      case Right(topLevel) =>
        assertEquals(Eval(topLevel, env, Ast.Ref("result")), Left(expected))
      case Left(error) =>
        fail("Could not parse expression: " + error)
    }
  }
}
