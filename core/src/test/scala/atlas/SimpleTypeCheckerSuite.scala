// package atlas

// import atlas.syntax._
// import minitest._

// object SimpleTypeCheckerSuite extends SimpleTestSuite {
//   test("constant") {
//     assertSuccess(expr"true", BoolType)
//   }

//   test("cond") {
//     assertSuccess(expr"if true then 123 else 456", IntType)
//     assertFailure(expr"if true then 123 else '456'", TypeMismatch(IntType, StrType))
//     assertFailure(expr"if true then '123' else 456", TypeMismatch(StrType, IntType))
//   }

//   test("infix") {
//     assertSuccess(expr"1 + 1", IntType)
//     assertSuccess(expr"1 + 1.0", DblType)
//     assertSuccess(expr"'1' + '1'", StrType)
//     assertFailure(expr"1 + '1'", InfixNotDefined(InfixOp.Add, IntType, StrType))
//   }

//   test("prefix") {
//     assertSuccess(expr"!true", BoolType)
//     assertSuccess(expr"!false", BoolType)
//     assertFailure(expr"!'1'", PrefixNotDefined(PrefixOp.Not, StrType))

//     assertSuccess(expr"+1", IntType)
//     assertSuccess(expr"+1.0", DblType)
//     assertFailure(expr"+'1'", PrefixNotDefined(PrefixOp.Pos, StrType))

//     assertSuccess(expr"-1", IntType)
//     assertSuccess(expr"-1.0", DblType)
//     assertFailure(expr"-'1'", PrefixNotDefined(PrefixOp.Neg, StrType))
//   }

//   test("cast") {
//     assertSuccess(expr"1 : Int", IntType)
//     assertSuccess(expr"1 : Real", DblType)
//     assertFailure(expr"1.0 : Int", TypeMismatch(IntType, DblType))
//     assertFailure(expr"(1 + 1.0) : Int", TypeMismatch(IntType, DblType))
//     assertSuccess(expr"(1 + 1.0) : Real", DblType)
//   }

//   test("defn") {
//     assertSuccess(expr"do let a = 1; a end", IntType)
//     assertSuccess(expr"do let a: Int = 1; a end", IntType)
//     assertSuccess(expr"do let a: Real = 1; a end", DblType)
//     assertFailure(expr"do let a: Int = 1.0; a end", TypeMismatch(IntType, DblType))

//     assertSuccess(expr"do let a = 1; let b = 1.0; a + b end", DblType)
//     assertFailure(expr"do let a = 1; let b = 'foo'; a + b end", InfixNotDefined(InfixOp.Add, IntType, StrType))
//   }

//   test("func") {
//     assertSuccess(expr"(a, b) -> a + b", NullType)
//   }

//   // test("native functions") {
//   //   // TODO: This should be generic!
//   //   val expected = Func(List(Func(List(Top), Top), Arr(Top)), Arr(Top))

//   //   assertSuccess(expr"map", expected, Env.basic)
//   // }

//   def assertSuccess(expr: Expr, expected: Type, env: Env = Env.create): Unit =
//     assertEquals(TypeChecker(expr, env), Right(expected))

//   def assertFailure(expr: Expr, expected: TypeError, env: Env = Env.create): Unit =
//     assertEquals(TypeChecker(expr, env), Left(expected))
// }
