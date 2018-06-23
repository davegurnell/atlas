package atlas

import atlas.syntax._
import cats.{Eval, MonadError}
import cats.data.EitherT
import cats.implicits._
import minitest._
import minitest.api.SourceLocation

object DesugarerSuite extends SimpleTestSuite {
  test("paren") {
    assertSuccess(
      expr"a + b + c",
      AppExpr(
        RefExpr("infix:+"),
        List(
          AppExpr(
            RefExpr("infix:+"),
            List(
              RefExpr("a"),
              RefExpr("b"))),
          RefExpr("c"))))

    assertSuccess(
      expr"(a + b) + c",
      AppExpr(
        RefExpr("infix:+"),
        List(
          AppExpr(
            RefExpr("infix:+"),
            List(
              RefExpr("a"),
              RefExpr("b"))),
          RefExpr("c"))))

    assertSuccess(
      expr"a + (b + c)",
      AppExpr(
        RefExpr("infix:+"),
        List(
          RefExpr("a"),
          AppExpr(
            RefExpr("infix:+"),
            List(
              RefExpr("b"),
              RefExpr("c"))))))
  }

  test("func") {
    assertSuccess(
      expr"a -> a",
      FuncExpr(
        List(RefType("?0"), RefType("?1")),
        List(FuncArg("a", RefType("?0"))),
        RefType("?1"),
        RefExpr("a")))

    assertSuccess(
      expr"(a: Int) -> a",
      FuncExpr(
        List(RefType("?0")),
        List(FuncArg("a", IntType)),
        RefType("?0"),
        RefExpr("a")))

    assertSuccess(
      expr"( a, b ) -> a + b",
      FuncExpr(
        List(RefType("?0"), RefType("?1"), RefType("?2")),
        List(
          FuncArg("a", RefType("?0")),
          FuncArg("b", RefType("?1"))),
        RefType("?2"),
        AppExpr(
          RefExpr("infix:+"),
          List(
            RefExpr("a"),
            RefExpr("b")))))

    assertSuccess(
      expr"( a , b : String ) : Int -> a + b",
      FuncExpr(
        List(RefType("?0")),
        List(
          FuncArg("a", RefType("?0")),
          FuncArg("b", StrType)),
        IntType,
        AppExpr(
          RefExpr("infix:+"),
          List(
            RefExpr("a"),
            RefExpr("b")))))

    assertSuccess(
      expr"(a:Int,b):Real->a+b",
      FuncExpr(
        List(RefType("?0")),
        List(
          FuncArg("a", IntType),
          FuncArg("b", RefType("?0"))),
        DblType,
        AppExpr(
          RefExpr("infix:+"),
          List(
            RefExpr("a"),
            RefExpr("b")))))

    assertSuccess(
      expr"a -> b -> a + b",
      FuncExpr(
        List(RefType("?0"), RefType("?1")),
        List(FuncArg("a", RefType("?0"))),
        RefType("?1"),
        FuncExpr(
          List(RefType("?2"), RefType("?3")),
          List(FuncArg("b", RefType("?2"))),
          RefType("?3"),
          AppExpr(
            RefExpr("infix:+"),
            List(
              RefExpr("a"),
              RefExpr("b"))))))

    assertSuccess(
      expr"a -> b -> a.c + b.d",
      FuncExpr(
        List(RefType("?0"), RefType("?1")),
        List(FuncArg("a", RefType("?0"))),
        RefType("?1"),
        FuncExpr(
          List(RefType("?2"), RefType("?3")),
          List(FuncArg("b", RefType("?2"))),
          RefType("?3"),
          AppExpr(
            RefExpr("infix:+"),
            List(
              SelectExpr(RefExpr("a"), "c"),
              SelectExpr(RefExpr("b"), "d"))))))
  }

  test("cond") {
    assertSuccess(
      expr"if true then 123 else 456",
      CondExpr(
        BoolExpr(true),
        IntExpr(123),
        IntExpr(456)))
  }

  test("cast") {
    assertSuccess(
      expr"123 : Int",
      CastExpr(IntExpr(123), IntType))

    assertSuccess(
      expr"123 + 234: Int",
      AppExpr(
        RefExpr("infix:+"),
        List(
          IntExpr(123),
          CastExpr(IntExpr(234), IntType))))

    assertSuccess(
      expr"(123 + 234): Int",
      CastExpr(
        AppExpr(
          RefExpr("infix:+"),
          List(
            IntExpr(123),
            IntExpr(234))),
        IntType))

    assertSuccess(
      expr"123 : Int | String",
      CastExpr(
        IntExpr(123),
        IntType | StrType))

    assertSuccess(
      expr"123 : (Int | String)",
      CastExpr(
        IntExpr(123),
        IntType | StrType))
  }

  test("obj") {
    assertSuccess(
      expr"{ a: 123, b: 456 }",
      ObjExpr(List(
        "a" -> IntExpr(123),
        "b" -> IntExpr(456))))
  }

  test("arr") {
    assertSuccess(
      expr"[ 123, 456 ]",
      ArrExpr(List(
        IntExpr(123),
        IntExpr(456))))
  }

  test("app") {
    assertSuccess(
      expr"add ( a , b , c )",
      AppExpr(
        RefExpr("add"),
        List(
          RefExpr("a"),
          RefExpr("b"),
          RefExpr("c"))))

    assertSuccess(
      expr"add(a,b,c)",
      AppExpr(
        RefExpr("add"),
        List(
          RefExpr("a"),
          RefExpr("b"),
          RefExpr("c"))))
  }

  test("prefix") {
    assertSuccess(
      expr"- a",
      AppExpr(
        RefExpr("prefix:-"),
        List(RefExpr("a"))))

    assertSuccess(
      expr"+a",
      AppExpr(
        RefExpr("prefix:+"),
        List(RefExpr("a"))))

    assertSuccess(
      expr"!a",
      AppExpr(
        RefExpr("prefix:!"),
        List(RefExpr("a"))))

    assertSuccess(
      expr"+ a + + b",
      AppExpr(
        RefExpr("infix:+"),
        List(
          AppExpr(
            RefExpr("prefix:+"),
            List(RefExpr("a"))),
          AppExpr(
            RefExpr("prefix:+"),
            List(RefExpr("b"))))))
  }

  test("infix") {
    assertSuccess(expr"a || b", AppExpr(RefExpr("infix:||"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a && b", AppExpr(RefExpr("infix:&&"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a == b", AppExpr(RefExpr("infix:=="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a != b", AppExpr(RefExpr("infix:!="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a > b", AppExpr(RefExpr("infix:>"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a < b", AppExpr(RefExpr("infix:<"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a >= b", AppExpr(RefExpr("infix:>="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a <= b", AppExpr(RefExpr("infix:<="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a + b", AppExpr(RefExpr("infix:+"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a - b", AppExpr(RefExpr("infix:-"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a * b", AppExpr(RefExpr("infix:*"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a / b", AppExpr(RefExpr("infix:/"), List(RefExpr("a"), RefExpr("b"))))

    assertSuccess(expr"a||b", AppExpr(RefExpr("infix:||"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a&&b", AppExpr(RefExpr("infix:&&"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a==b", AppExpr(RefExpr("infix:=="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a!=b", AppExpr(RefExpr("infix:!="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a>b", AppExpr(RefExpr("infix:>"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a<b", AppExpr(RefExpr("infix:<"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a>=b", AppExpr(RefExpr("infix:>="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a<=b", AppExpr(RefExpr("infix:<="), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a+b", AppExpr(RefExpr("infix:+"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a-b", AppExpr(RefExpr("infix:-"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a*b", AppExpr(RefExpr("infix:*"), List(RefExpr("a"), RefExpr("b"))))
    assertSuccess(expr"a/b", AppExpr(RefExpr("infix:/"), List(RefExpr("a"), RefExpr("b"))))

    assertSuccess(
      expr"a + b + c",
      AppExpr(
        RefExpr("infix:+"),
        List(
          AppExpr(
            RefExpr("infix:+"),
            List(
              RefExpr("a"),
              RefExpr("b"))),
          RefExpr("c"))))

    assertSuccess(
      expr"a * b + c",
      AppExpr(
        RefExpr("infix:+"),
        List(
          AppExpr(
            RefExpr("infix:*"),
            List(
              RefExpr("a"),
              RefExpr("b"))),
          RefExpr("c"))))

    assertSuccess(
      expr"a + b * c",
      AppExpr(
        RefExpr("infix:+"),
        List(
          RefExpr("a"),
          AppExpr(
            RefExpr("infix:*"),
            List(
              RefExpr("b"),
              RefExpr("c"))))))

    assertSuccess(
      expr"( a + b ) * c",
      AppExpr(
        RefExpr("infix:*"),
        List(
          AppExpr(
            RefExpr("infix:+"),
            List(
              RefExpr("a"),
              RefExpr("b"))),
          RefExpr("c"))))

    assertSuccess(
      expr"a * (b + c)",
      AppExpr(
        RefExpr("infix:*"),
        List(
          RefExpr("a"),
          AppExpr(
            RefExpr("infix:+"),
            List(
              RefExpr("b"),
              RefExpr("c"))))))

    assertSuccess(
      expr"a <= b && c >= d",
      AppExpr(
        RefExpr("infix:&&"),
        List(
          AppExpr(
            RefExpr("infix:<="),
            List(
              RefExpr("a"),
              RefExpr("b"))),
          AppExpr(
            RefExpr("infix:>="),
            List(
              RefExpr("c"),
              RefExpr("d"))))))

    assertSuccess(
      expr"(a) + (b)",
      AppExpr(
        RefExpr("infix:+"),
        List(
          RefExpr("a"),
          RefExpr("b"))))

    assertSuccess(
      expr"+a + +b",
      AppExpr(
        RefExpr("infix:+"),
        List(
          AppExpr(
            RefExpr("prefix:+"),
            List(RefExpr("a"))),
          AppExpr(
            RefExpr("prefix:+"),
            List(RefExpr("b"))))))
  }

  test("select") {
    assertSuccess(
      expr"a . b",
      SelectExpr(RefExpr("a"), "b"))

    assertSuccess(
      expr"a.b.c",
      SelectExpr(SelectExpr(RefExpr("a"), "b"), "c"))

    assertSuccess(
      expr"a.b+c.d",
      AppExpr(
        RefExpr("infix:+"),
        List(
          SelectExpr(RefExpr("a"), "b"),
          SelectExpr(RefExpr("c"), "d"))))
  }

  test("block") {
    assertSuccess(
      expr"do a end",
      RefExpr("a"))

    assertSuccess(
      expr"do a; b; c end",
      BlockExpr(
        List(
          ExprStmt(RefExpr("a")),
          ExprStmt(RefExpr("b"))),
        RefExpr("c")))
  }

  def assertSuccess(expr: ExprStx, expected: Expr)(implicit loc: SourceLocation): Unit =
    assertEquals(expr.desugar, expected)
}
