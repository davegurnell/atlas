package atlas

import atlas.syntax._
import cats.Eval
import cats.data.EitherT
import cats.implicits._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class InterpreterBenchmark {
  def sum(n: Int): Int = {
    val program =
      prog"""
      let sum = n ->
        if n <= 1 then 1 else n + sum(n - 1)

      sum(n)
      """

    val env =
      SyncInterpreter.createEnv.set("n", n)

    val res =
      SyncInterpreter.evalAs[Int](program, env).value.value

    res.right.get
  }

  def fib(n: Int): Int = {
    val program =
      prog"""
      let fib = n ->
        if n <= 1 then 1 else fib(n - 1) + fib(n - 2)

      fib(n)
      """

    val env =
      SyncInterpreter.createEnv.set("n", n)

    val res =
      SyncInterpreter.evalAs[Int](program, env).value.value

    res.right.get
  }


  @Benchmark def sum0010() = sum(10)
  @Benchmark def sum0025() = sum(25)
  @Benchmark def sum0050() = sum(50)
  @Benchmark def sum0100() = sum(100)
  @Benchmark def sum0250() = sum(250)
  @Benchmark def sum0500() = sum(500)
  @Benchmark def sum1000() = sum(1000)

  @Benchmark def fib005() = fib(5)
  @Benchmark def fib010() = fib(10)
  @Benchmark def fib015() = fib(15)
  @Benchmark def fib025() = fib(25)
  // @Benchmark def fib050() = fib(50)
  // @Benchmark def fib100() = fib(100)
}