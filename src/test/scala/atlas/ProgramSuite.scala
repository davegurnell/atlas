package atlas

import cats.{Eval, MonadError}
import cats.data.EitherT
import cats.implicits._
import minitest._
import syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object SyncProgramSuite extends ProgramSuite(Interpreter.sync) {
  def toEither[A](either: EitherT[Eval, RuntimeError, A]): Either[RuntimeError, A] =
    either.value.value
}

object AsyncProgramSuite extends ProgramSuite(Interpreter.async) {
  def toEither[A](eitherT: EitherT[Future, RuntimeError, A]): Either[RuntimeError, A] =
    Await.result(eitherT.value, 1.second)
}

abstract class ProgramSuite[F[_]](interpreter: Interpreter[F])(implicit monad: MonadError[F, RuntimeError]) extends SimpleTestSuite {
  import interpreter.native

  test("recursive odd/even") {
    val code = prog"""
      let even = n -> if n == 0 then true else odd(n - 1)
      let odd  = n -> if n == 0 then false else even(n - 1)

      even(10)
      """
    val env = Env.create[F]
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
    val env = Env.create[F]
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
    val env = Env.create[F]
    val expected = 55

    assertSuccess(prog, env, expected)
  }

  test("map, filter, flatten") {
    val prog = prog"""
      let inBounds = n ->
        n > 1 && n < 20

      let double = n ->
        [ n, n * 2 ]

      let values =
        [1, 5, 7]

      filter(inBounds, flatten(map(double, values)))
      """
    val env = interpreter.basicEnv
    val expected = List(2, 5, 10, 7, 14)

    assertSuccess(prog, env, expected)
  }

  test("comments") {
    val prog = prog"""
      # Comment
      let# Comment
      double# Comment
      =# Comment
      n# Comment
      -># Comment
      n# Comment
      *# Comment
      2# Comment
      double# Comment
      (# Comment
      21# Comment
      )# Comment
      # Comment
      """
    val env = Env.create[F]
    val expected = 42

    assertSuccess(prog, env, expected)
  }

  test("native functions") {
    val prog = prog"""average(10, 5)"""
    val env = Env.create[F]
      .set("average", native((a: Double, b: Double) => (a + b) / 2))
    val expected = 7.5

    assertSuccess(prog, env, expected)
  }

  test("native functions with exceptions") {
    val prog = prog"""average(10, 5)"""
    val exn = new Exception("Badness")
    val env = Env.create[F]
      .set("average", native((a: Double, b: Double) => { if(a > b) throw exn ; 0 }))
    val expected = RuntimeError("Error executing native code", Some(exn))

    assertFailure(prog, env, expected)
  }

  def assertSuccess[A](prog: Expr, env: Env[F], expected: A)(implicit dec: ValueDecoder[F, A]): Unit =
    assertEquals(toEither(interpreter.evalAs[A](prog, env)), Right(expected))

  def assertFailure(prog: Expr, env: Env[F], expected: RuntimeError): Unit =
    assertEquals(toEither(interpreter.eval(prog, env)), Left(expected))

  def toEither[A](value: F[A]): Either[RuntimeError, A]
}
