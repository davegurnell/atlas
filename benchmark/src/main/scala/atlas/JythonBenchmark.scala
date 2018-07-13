package example

import java.io._
import java.util.Properties
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.python.core._
import org.python.util.PythonInterpreter
import unindent._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class JythonBenchmark {
  def createInterpreter: PythonInterpreter = {
    val props = new Properties()
    props.setProperty("python.import.site", "false")
    val systemState = PySystemState.doInitialize(props, new Properties(), Array[String](), null, null)

    new PythonInterpreter(null, systemState)
  }

  def sum(n: Int): Unit = {
    val interpreter = createInterpreter

    val code =
      i"""
      def sum(n):
          if n <= 1:
              return 1
          else:
              return n + sum(n - 1)

      ans = sum($n)
      """

    interpreter.exec(code)
    assert(interpreter.get("ans").isInstanceOf[PyInteger])
  }

  def fib(n: Int): Unit = {
    val interpreter = createInterpreter

    val code =
      i"""
      def fib(n):
          if n <= 1:
              return 1
          else:
              return fib(n - 1) + fib(n - 2)

      ans = fib($n)
      """

    println(code)
    // interpreter.exec(code)
    // println(interpreter.get("ans"))
    // assert(interpreter.get("ans").isInstanceOf[PyInteger])
  }

  @Benchmark def sum0010() = sum(10)
  @Benchmark def sum0025() = sum(25)
  @Benchmark def sum0050() = sum(50)
  @Benchmark def sum0100() = sum(100)
  // @Benchmark def sum0250() = sum(250)
  // @Benchmark def sum0500() = sum(500)
  // @Benchmark def sum1000() = sum(1000)

  @Benchmark def fib005() = fib(5)
  @Benchmark def fib010() = fib(10)
  @Benchmark def fib015() = fib(15)
  // @Benchmark def fib025() = fib(25)
}
