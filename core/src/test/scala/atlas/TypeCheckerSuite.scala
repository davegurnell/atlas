// package atlas

// import atlas.syntax._
// import cats.{Eval, MonadError}
// import cats.data.EitherT
// import cats.implicits._
// import minitest._

// object TypeCheckerSuite extends SimpleTestSuite {
//   test("lit") {
//     assertSuccess(expr"true",   BoolType)
//     assertSuccess(expr"123",    IntType)
//     assertSuccess(expr"123.0",  DblType)
//     assertSuccess(expr"'Dave'", StrType)
//     assertSuccess(expr"null",   NullType)
//   }

//   test("let / ref") {
//     assertSuccess(
//       prog"""
//       let x = true
//       x
//       """,
//       BoolType)

//     assertSuccess(
//       prog"""
//       let x = 123
//       x
//       """,
//       IntType)

//     assertSuccess(
//       prog"""
//       let x = 123.0
//       x
//       """,
//       DblType)

//     assertSuccess(
//       prog"""
//       let x = 'Dave'
//       x
//       """,
//       StrType)

//     assertSuccess(
//       prog"""
//       let x: Int | String = 'Dave'
//       x
//       """,
//       IntType | StrType)

//     assertSuccess(
//       prog"""
//       let x: Int | String = if true then 'Dave' else 123
//       x
//       """,
//       IntType | StrType)

//     assertFailure(
//       prog"""
//       let x: Int | String = true
//       x
//       """,
//       TypeError.typeMismatch(IntType | StrType, BoolType))

//     assertSuccess(
//       prog"""
//       let x = null
//       x
//       """,
//       NullType)

//     assertSuccess(
//       prog"""
//       let x: String? = null
//       x
//       """,
//       StrType.?)

//     assertFailure(
//       prog"""
//       let x: String = null
//       x
//       """,
//       TypeError.typeMismatch(StrType, NullType))
//   }

//   test("type let") {
//     assertSuccess(
//       prog"""
//       type A = Int
//       type B = String
//       type C = A -> B
//       type D = (A, B) -> A
//       let x: (C | D)? = null
//       x
//       """,
//       FuncType(List(IntType), StrType) | FuncType(List(IntType, StrType), IntType).?
//     )

//     assertSuccess(
//       prog"""
//       type A = Int
//       let x: A? = null
//       x
//       """,
//       IntType.?
//     )
//   }

//   test("func") {
//     assertSuccess(
//       expr"""(a: Int, b: String) -> if true then a else b""",
//       IntType | StrType)

//     assertSuccess(
//       expr"""(a: Int, b: String): (Int | String) -> if true then a else b""",
//       IntType | StrType)

//     assertSuccess(
//       expr"""(a: Int, b: String): (Int? | String) -> if true then a else b""",
//       IntType | StrType | NullType)

//     assertFailure(
//       expr"""(a: Int, b: String) -> if true then a else c""",
//       TypeError.variableNotFound("c"))

//     assertFailure(
//       expr"""(a: A, b: String) -> if true then a else b""",
//       TypeError.typeNotFound("A"))
//   }

//   test("cond") {
//     assertSuccess(prog"""if true then 123 else 456""",    IntType)
//     assertSuccess(prog"""if true then 123 else 'Dave'""", IntType | StrType)
//     assertSuccess(prog"""if true then 123 else null""",   IntType.?)
//     assertFailure(prog"""if 123  then 123 else null""",   TypeError.typeMismatch(BoolType, IntType))
//   }

//   test("cast") {
//     assertSuccess(expr"""123 : Int | String""", IntType | StrType)
//     assertFailure(expr"""(if true then 123 else 'Dave') : Int""", TypeError.typeMismatch(IntType, IntType | StrType))
//   }

//   test("array") {
//     assertSuccess(expr"""[123, 234, 345]""",     ArrType(IntType))
//     assertSuccess(expr"""[123, true, 'Dave']""", ArrType(IntType | BoolType | StrType))
//     assertSuccess(expr"""[]""",                  ArrType(Type.emptyUnion))
//   }

//   def assertSuccess(expr: Expr, expected: Type): Unit =
//     assertEquals(TypeChecker.check(expr), Right(expected))

//   def assertFailure(expr: Expr, expected: TypeError): Unit =
//     assertEquals(TypeChecker.check(expr), Left(expected))
// }
