package atlas

import minitest._
import syntax._
import unindent._

object ProgEvalSuite extends SimpleTestSuite {
  test("recursive odd/even") {
    val code = prog"""
      let even = n -> if n == 0 then true else odd(n - 1)
      let odd  = n -> if n == 0 then false else even(n - 1)

      even(10)
      """
    val env = Env.create
    val expected = true

    assertSuccess(code, env, expected)
  }

  test("factorial") {
    val prog = prog"""
      let factorial = n ->
        if n <= 1
        then 1
        else n * factorial(n - 1)

      factorial(10)
      """
    val env = Env.create
    val expected = (1 to 10).foldLeft(1)(_ * _)

    assertSuccess(prog, env, expected)
  }

  test("fib") {
    val prog = prog"""
      let fib = n ->
        if n <= 2
        then 1
        else fib(n - 1) + fib(n - 2)

      fib(10)
      """
    val env = Env.create
    val expected = 55

    assertSuccess(prog, env, expected)
  }

  def assertSuccess[A](prog: Ast.Expr, env: Env, expected: A)(implicit dec: ValueDecoder[A]): Unit =
    assertEquals(Eval(prog, env).flatMap(dec.apply), Right(expected))

  def assertFailure(prog: Ast.Expr, env: Env, expected: Eval.Error): Unit =
    assertEquals(Eval(prog, env), Left(expected))
}
