package atlas

import atlas.syntax._
import minitest._

object SimpleTypeCheckerSuite extends SimpleTestSuite {
  import Type._

  test("constant") {
    assertSuccess(expr"true", Type.Bool)
  }

  test("cond") {
    assertSuccess(expr"if true then 123 else 456", Type.Intr)
    assertSuccess(expr"if true then 123 else '456'", Union(Set(Intr, Str)))
    assertSuccess(expr"if true then '123' else 456", Union(Set(Intr, Str)))
  }

  test("infix") {
    assertSuccess(expr"1 + 1", Intr)
    assertSuccess(expr"1 + 1.0", Real)
    assertSuccess(expr"'1' + '1'", Str)
    assertFailure(expr"1 + '1'", InfixNotDefined(InfixOp.Add, Intr, Str))
  }

  test("prefix") {
    assertSuccess(expr"!true", Bool)
    assertSuccess(expr"!false", Bool)
    assertFailure(expr"!'1'", PrefixNotDefined(PrefixOp.Not, Str))

    assertSuccess(expr"+1", Intr)
    assertSuccess(expr"+1.0", Real)
    assertFailure(expr"+'1'", PrefixNotDefined(PrefixOp.Pos, Str))

    assertSuccess(expr"-1", Intr)
    assertSuccess(expr"-1.0", Real)
    assertFailure(expr"-'1'", PrefixNotDefined(PrefixOp.Neg, Str))
  }

  test("cast") {
    assertSuccess(expr"1 : Int", Type.Intr)
    assertSuccess(expr"1 : Real", Type.Real)
    assertFailure(expr"1.0 : Int", TypeMismatch(Type.Intr, Type.Real))
    assertFailure(expr"(1 + 1.0) : Int", TypeMismatch(Type.Intr, Type.Real))
    assertSuccess(expr"(1 + 1.0) : Real", Type.Real)
  }

  test("defn") {
    assertSuccess(expr"do let a = 1; a end", Type.Intr)
    assertSuccess(expr"do let a: Int = 1; a end", Type.Intr)
    assertSuccess(expr"do let a: Real = 1; a end", Type.Real)
    assertFailure(expr"do let a: Int = 1.0; a end", TypeMismatch(Type.Intr, Type.Real))

    assertSuccess(expr"do let a = 1; let b = 1.0; a + b end", Type.Real)
    assertFailure(expr"do let a = 1; let b = 'foo'; a + b end", InfixNotDefined(InfixOp.Add, Intr, Str))
  }

  test("native functions") {
    // TODO: This should be generic!
    val expected = Func(List(Func(List(Top), Top), Arr(Top)), Arr(Top))

    assertSuccess(expr"map", expected, Env.basic)
  }

  def assertSuccess(expr: Ast.Expr, expected: Type, env: Env = Env.create): Unit =
    assertEquals(TypeChecker(expr, env), Right(expected))

  def assertFailure(expr: Ast.Expr, expected: TypeError, env: Env = Env.create): Unit =
    assertEquals(TypeChecker(expr, env), Left(expected))
}
