package example

import java.io._
import java.util.Properties
import java.util.concurrent.TimeUnit
import javax.script.ScriptEngine
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
import org.openjdk.jmh.annotations._
import unindent._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class NashornBenchmark {
  def createInterpreter: ScriptEngine = {
    val factory = new NashornScriptEngineFactory()
    Option(factory.getScriptEngine("--no-java")).get
  }

  def sum(n: Int): Any = {
    val interpreter = createInterpreter
    val result = interpreter.eval {
      i"""
      function fact(n) {
        return n <= 1
          ? 1
          : n + fact(n - 1);
      }

      fact($n)
      """
    }

    assert(result.isInstanceOf[java.lang.Double])
  }

  def fib(n: Int): Any = {
    val interpreter = createInterpreter
    val result = interpreter.eval {
      i"""
      function fib(n) {
        return n <= 1
          ? 1
          : fib(n - 1) + fib(n - 2);
      }

      fib($n)
      """
    }

    assert(result.isInstanceOf[java.lang.Double])
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
