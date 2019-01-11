package atlas

import atlas.syntax._
import cats._
import cats.data._
import cats.implicits._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Scope => JmhScope, _}
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class SyncBenchmark extends InterpreterBenchmark[EitherT[Eval, RuntimeError, ?]] {
  def monadError = MonadError[EitherT[Eval, RuntimeError, ?], RuntimeError]

  def unpack[A](either: EitherT[Eval, RuntimeError, A]): A =
    either.value.value.right.get
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class AsyncBenchmark extends InterpreterBenchmark[EitherT[Future, RuntimeError, ?]] {
  def monadError = MonadError[EitherT[Future, RuntimeError, ?], RuntimeError]

  def unpack[A](eitherT: EitherT[Future, RuntimeError, A]): A =
    Await.result(eitherT.value, 60.seconds).right.get
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class IdBenchmark extends InterpreterBenchmark[Either[RuntimeError, ?]] {
  def monadError = MonadError[Either[RuntimeError, ?], RuntimeError]

  def unpack[A](either: Either[RuntimeError, A]): A =
    either.right.get
}

abstract class InterpreterBenchmark[F[_]] {
  implicit def monadError: MonadError[F, RuntimeError]

  def unpack[A](value: F[A]): A

  def sum(n: Int): Int = {
    val program =
      prog"""
      let sum = n ->
        if n <= 1 then 1 else n + sum(n - 1)

      sum(n)
      """

    val env =
      Env.create[F].set("n", n)

    unpack(Interpreter.evalAs[F, Int](program, env))
  }

  def fib(n: Int): Int = {
    val program =
      prog"""
      let fib = n ->
        if n <= 1 then 1 else fib(n - 1) + fib(n - 2)

      fib(n)
      """

    val env =
      Env.create[F].set("n", n)

    unpack(Interpreter.evalAs[F, Int](program, env))
  }


  // @Benchmark def sum0010() = sum(10)
  // @Benchmark def sum0025() = sum(25)
  // @Benchmark def sum0050() = sum(50)
  @Benchmark def sum0100() = sum(100)
  // @Benchmark def sum0250() = sum(250)
  // @Benchmark def sum0500() = sum(500)
  // @Benchmark def sum1000() = sum(1000)

  // @Benchmark def fib005() = fib(5)
  // @Benchmark def fib010() = fib(10)
  @Benchmark def fib015() = fib(15)
  // @Benchmark def fib025() = fib(25)
  // @Benchmark def fib050() = fib(50)
  // @Benchmark def fib100() = fib(100)
}