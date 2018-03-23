package atlas

import atlas.syntax._
import minitest._

object TypeAnnotatorSuite extends SimpleTestSuite {
  test("constant") {
    assertSuccess(expr"true", TBoolExpr(v(0), true))
  }

  test("cond") {
    assertSuccess(
      expr"if true then 123 else 456",
      TCondExpr(
        v(0),
        TBoolExpr(v(1), true),
        TIntExpr(v(2), 123),
        TIntExpr(v(3), 456))
    )
  }

  test("infix") {
    assertSuccess(
      expr"123 + 456 + 789",
      TInfixExpr(
        v(0),
        InfixOp.Add,
        TInfixExpr(
          v(1),
          InfixOp.Add,
          TIntExpr(v(2), 123),
          TIntExpr(v(3), 456)),
        TIntExpr(v(4), 789)))
  }

  test("prefix") {
    assertSuccess(
      expr"!!true",
      TPrefixExpr(
        v(0),
        PrefixOp.Not,
        TPrefixExpr(
          v(1),
          PrefixOp.Not,
          TBoolExpr(v(2), true))))
  }

  test("cast") {
    assertSuccess(
      expr"1 : Int : Real",
      TCastExpr(
        v(0),
        TCastExpr(
          v(1),
          TIntExpr(v(2), 1),
          IntType),
        DblType))
  }

  test("block") {
    assertSuccess(
      expr"do 1; 2; 3 end",
      TBlockExpr(
        v(0),
        List(
          TIntExpr(v(1), 1),
          TIntExpr(v(2), 2)),
        TIntExpr(v(3), 3)))
  }

  test("let / ref") {
    assertSuccess(
      expr"do let a = 1; a end",
      TBlockExpr(
        v(0),
        List(
          TLetExpr(
            v(1),
            "a",
            None,
            TIntExpr(v(2), 1))),
        TRefExpr(v(1), "a")))
  }

  test("func") {
    assertSuccess(
      expr"(a, b) -> a + b",
      TFuncExpr(
        v(0),
        List(
          TFuncArg(v(1), "a", None),
          TFuncArg(v(2), "b", None)),
        None,
        TInfixExpr(
          v(3),
          InfixOp.Add,
          TRefExpr(v(1), "a"),
          TRefExpr(v(2), "b"))))
  }

  test("block scope") {
    assertSuccess(
      expr"""
      do
        let a = 1
        do
          let a = 'hi'
          a
        end
        a
      end
      """,
      TBlockExpr(
        v(0),
        List(
          TLetExpr(v(1), "a", None, TIntExpr(v(2), 1)),
          TBlockExpr(
            v(3),
            List(
              TLetExpr(v(4), "a", None, TStrExpr(v(5), "hi"))),
            TRefExpr(v(4), "a"))),
        TRefExpr(v(1), "a")))
  }

  test("func scope") {
    assertSuccess(
      expr"""
      do
        let a = a -> a
        a
      end
      """,
      TBlockExpr(
        v(0),
        List(
          TLetExpr(
            v(1),
            "a",
            None,
            TFuncExpr(
              v(2),
              List(TFuncArg(v(3), "a", None)),
              None,
              TRefExpr(v(3), "a")))),
          TRefExpr(v(1), "a")))
  }

  test("mutual recursion") {
    assertSuccess(
      expr"""
      do
        let a = n -> b(n)
        let b = n -> a(n)
        a(1)
      end
      """,
      TBlockExpr(
        v(0),
        List(
          TLetExpr(
            v(1),
            "a",
            None,
            TFuncExpr(
              v(3),
              List(TFuncArg(v(4), "n", None)),
              None,
              TAppExpr(
                v(5),
                TRefExpr(v(2), "b"),
                List(TRefExpr(v(4), "n"))))),
        TLetExpr(
          v(2),
          "b",
          None,
          TFuncExpr(
            v(6),
            List(TFuncArg(v(7), "n", None)),
            None,
            TAppExpr(
              v(8),
              TRefExpr(v(1), "a"),
              List(
                TRefExpr(v(7), "n")))))),
        TAppExpr(
          v(9),
          TRefExpr(v(1), "a"),
          List(TIntExpr(v(10), 1)))))
  }

  def assertSuccess(expr: Expr, expected: TExpr, env: Env = Env.create): Unit =
    assertEquals(TypeAnnotator(expr), Right(expected))

  def assertFailure(expr: Expr, expected: TypeError, env: Env = Env.create): Unit =
    assertEquals(TypeAnnotator(expr), Left(expected))

  def v(id: Int): TypeVar =
    TypeVar(id)
}
