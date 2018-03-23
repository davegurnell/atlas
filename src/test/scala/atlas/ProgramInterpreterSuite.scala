// package atlas

// import minitest._
// import syntax._

// object ProgramInterpreterSuite extends SimpleTestSuite {
//   test("recursive odd/even") {
//     val code = prog"""
//       let even = n -> if n == 0 then true else odd(n - 1)
//       let odd  = n -> if n == 0 then false else even(n - 1)

//       even(10)
//       """
//     val env = Env.create
//     val expected = true

//     assertSuccess(code, env, expected)
//   }

//   test("factorial") {
//     val prog = prog"""
//       let factorial = n ->
//         if n <= 1
//         then 1
//         else n * factorial(n - 1)

//       factorial(10)
//       """
//     val env = Env.create
//     val expected = (1 to 10).foldLeft(1)(_ * _)

//     assertSuccess(prog, env, expected)
//   }

//   test("fib") {
//     val prog = prog"""
//       let fib = n ->
//         if n <= 2
//         then 1
//         else fib(n - 1) + fib(n - 2)

//       fib(10)
//       """
//     val env = Env.create
//     val expected = 55

//     assertSuccess(prog, env, expected)
//   }

//   test("map, filter, flatten") {
//     val prog = prog"""
//       let inBounds = n ->
//         n > 1 && n < 20

//       let double = n ->
//         [ n, n * 2 ]

//       let values =
//         [1, 5, 7]

//       filter(inBounds, flatten(map(double, values)))
//       """
//     val env = Env.basic
//     val expected = List(2, 5, 10, 7, 14)

//     assertSuccess(prog, env, expected)
//   }

//   test("comments") {
//     val prog = prog"""
//       # Comment
//       let# Comment
//       double# Comment
//       =# Comment
//       n# Comment
//       -># Comment
//       n# Comment
//       *# Comment
//       2# Comment
//       double# Comment
//       (# Comment
//       21# Comment
//       )# Comment
//       # Comment
//       """
//     val env = Env.create
//     val expected = 42

//     assertSuccess(prog, env, expected)
//   }

//   test("native functions") {
//     val prog = prog"""average(10, 5)"""
//     val env = Env.create
//       .set("average", (a: Double, b: Double) => (a + b) / 2)
//     val expected = 7.5

//     assertSuccess(prog, env, expected)
//   }

//   test("native functions with exceptions") {
//     val prog = prog"""average(10, 5)"""
//     val exn = new Exception("Badness")
//     val env = Env.create
//       .set("average", (a: Double, b: Double) => { if(a > b) throw exn ; 0 })
//     val expected = RuntimeError("Error in native function", Some(exn))

//     assertFailure(prog, env, expected)
//   }

//   def assertSuccess[A](prog: Expr, env: Env, expected: A)(implicit dec: ValueDecoder[A]): Unit =
//     assertEquals(Interpreter(prog, env).flatMap(dec.apply), Right(expected))

//   def assertFailure(prog: Expr, env: Env, expected: RuntimeError): Unit =
//     assertEquals(Interpreter(prog, env), Left(expected))
// }
