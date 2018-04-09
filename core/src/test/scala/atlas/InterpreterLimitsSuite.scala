package atlas

import cats.{Eval, MonadError}
import cats.data.EitherT
import cats.implicits._
import minitest._
import syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object SyncInterpreterLimitsSuite extends InterpreterLimitsSuite(Interpreter.sync) {
  def toEither[A](either: EitherT[Eval, RuntimeError, A]): Either[RuntimeError, A] =
    either.value.value
}

object AsyncInterpreterLimitsSuite extends InterpreterLimitsSuite(Interpreter.async) {
  def toEither[A](eitherT: EitherT[Future, RuntimeError, A]): Either[RuntimeError, A] =
    Await.result(eitherT.value, 1.second)
}

abstract class InterpreterLimitsSuite[F[_]](interpreter: Interpreter[F])(implicit monad: MonadError[F, RuntimeError]) extends InterpreterSuite[F](interpreter) {
  import interpreter.implicits._
  import interpreter.native

  test("program counter limit") {
    val code = prog"""
      let fact = n ->
        if n <= 1
        then 1
        else n * fact(n - 1)

      fact(1)
      """

    assertFailure(
      code,
      RuntimeError("Script exceeded program counter limit: 8"),
      limits = Limits.create.copy(pcLimit = Some(8)))

    assertSuccess(
      code,
      1,
      limits = Limits.create.copy(pcLimit = Some(9)))
  }

  test("runtime limit") {
    val code = prog"""
      passthru(() -> do
        delay(250)
        null
      end)
      """

    val env = Env.create[F]
      .set("passthru", native.pure { (callback: () => EvalStep[F, Unit]) => callback() })
      .set("delay", native { (millis: Int) => Thread.sleep(1L * millis) })

    assertFailure(
      code,
      RuntimeError("Script exceeded runtime limit: 100ms"),
      env = env,
      limits = Limits.create.copy(runtimeLimit = Some(100)))

    assertSuccess(
      code,
      (),
      env = env,
      limits = Limits.create.copy(runtimeLimit = Some(300)))
  }
}
