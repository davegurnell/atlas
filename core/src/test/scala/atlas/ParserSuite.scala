package atlas

import fastparse.all._
import minitest._
import unindent._

object UnitParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  test("digit") {
    object assert extends Assertions(digit)

    assert.complete("1", ())
    assert.failure("A", 0)
  }

  test("hexDigit") {
    object assert extends Assertions(hexDigit)

    assert.complete("1", ())
    assert.complete("A", ())
    assert.complete("f", ())
    assert.failure("G", 0)
  }

  test("newline") {
    object assert extends Assertions(newline)

    assert.complete("\r", ())
    assert.complete("\n", ())
    assert.complete("\r\n", ())
    assert.complete("\f", ())
    assert.partial("\n\r", (), 1)
  }

  test("whitespace") {
    object assert extends Assertions(whitespace)

    assert.complete(" ", ())
    assert.complete("\t", ())
    assert.failure("\r\n", 0)
    assert.failure("\n\r", 0)
  }

  test("ws") {
    object assert extends Assertions(ws)

    assert.complete(" \t \t ", ())
    assert.complete(" \n \n ", ())
    assert.complete(" \n \n \n ", ())
  }

  test("comment") {
    object assert extends Assertions(comment)

    assert.complete("# This is a comment", ())
    assert.partial("# This is a comment\n# So is this", (), 19)
  }

  test("escape") {
    object assert extends Assertions(escape)

    assert.complete("\\n", ())
    assert.complete("\\\\", ())
    assert.complete("\\\"", ())
    assert.failure("\\\n", 1)
  }
}

object TokenParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  test("boolean") {
    object assert extends Assertions(booleanToken)

    assert.complete("true", "true")
    assert.complete("false", "false")
    assert.partial("true false", "true", 4)
    assert.failure("truefalse", 0)
    assert.failure("maybe", 0)
  }

   test("intNumber") {
     object assert extends Assertions(intToken)

     assert.complete("123", "123")
     assert.partial("123.", "123", 3)
     assert.failure(".123", 0)
     assert.partial("123.456", "123", 3)
     assert.complete("+123", "+123")
     assert.failure("-.123", 1)
     assert.partial("123e456", "123", 3)
     assert.failure("-.123E-456", 1)
     assert.failure("letters", 0)
   }

  test("realNumber") {
    object assert extends Assertions(doubleToken)

    assert.failure("123", 0)
    assert.complete("123.", "123.")
    assert.complete(".123", ".123")
    assert.complete("123.456", "123.456")
    assert.failure("+123", 1)
    assert.complete("-.123", "-.123")
    assert.complete("123e456", "123e456")
    assert.complete("-.123E-456", "-.123E-456")
    assert.failure("letters", 0)
  }

  test("string") {
    object assert extends Assertions(stringToken)

    assert.complete("""'dave'""", "dave")
    assert.complete(""""has"""", "has")
    assert.complete(""""'escaped'"""", """'escaped'""")
    assert.complete("""'"escaped"'""", """"escaped"""")
    assert.partial("""'"abc'"def"""", """"abc""", 6)
  }

  test("ident") {
    object assert extends Assertions(ident)

    assert.complete("dave", "dave")
    assert.failure("if", 2)
    assert.complete("ifdave", "ifdave")
    assert.partial("dave was here", "dave", 4)
  }
}

object TypeParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  import PrefixOp._
  import InfixOp._

  object assert extends Assertions(tpe)

  test("literal") {
    assert.complete("Int", IntTypeStx)
    assert.complete("Real", DblTypeStx)
    assert.complete("String", StrTypeStx)
    assert.complete("Boolean", BoolTypeStx)
    assert.complete("Null", NullTypeStx)
  }

  test("ref") {
    assert.complete("A", RefTypeStx("A"))
  }

  test("func") {
    assert.complete(
      "[A, B, C](A, B) -> C",
      FuncTypeStx(List("A", "B", "C"), List(RefTypeStx("A"), RefTypeStx("B")), RefTypeStx("C")))

    assert.complete(
      "Int -> String",
      FuncTypeStx(Nil, List(IntTypeStx), StrTypeStx))

    assert.complete(
      "(Int, String) -> Boolean",
      FuncTypeStx(Nil, List(IntTypeStx, StrTypeStx), BoolTypeStx))

    assert.complete(
      "Int -> String -> Boolean",
      FuncTypeStx(
        Nil,
        List(IntTypeStx),
        FuncTypeStx(
          Nil,
          List(StrTypeStx),
          BoolTypeStx)))

    assert.complete(
      "(Int, Real -> Int) -> (String, Real -> String) -> Boolean",
      FuncTypeStx(
        Nil,
        List(IntTypeStx, FuncTypeStx(Nil, List(DblTypeStx), IntTypeStx)),
        FuncTypeStx(
          Nil,
          List(StrTypeStx, FuncTypeStx(Nil, List(DblTypeStx), StrTypeStx)),
          BoolTypeStx)))
  }

  test("union") {
    assert.complete(
      "Boolean | String",
      UnionTypeStx(BoolTypeStx, StrTypeStx))

    assert.complete(
      "Boolean | String | Null",
      UnionTypeStx(UnionTypeStx(BoolTypeStx, StrTypeStx), NullTypeStx))

    // TODO: Should function types be higher or lower precedence than union types?
    assert.complete(
      "Int -> Boolean | String",
      FuncTypeStx(Nil, List(IntTypeStx), UnionTypeStx(BoolTypeStx, StrTypeStx)))

    // TODO: Should function types be higher or lower precedence than union types?
    assert.complete(
      "(Int -> Boolean) | String",
      UnionTypeStx(ParenTypeStx(FuncTypeStx(Nil, List(IntTypeStx), BoolTypeStx)), StrTypeStx))
  }

  test("nullable") {
    assert.complete(
      "(String?)",
      ParenTypeStx(NullableTypeStx(StrTypeStx)))

    assert.complete(
      "(String -> Int?)",
      ParenTypeStx(FuncTypeStx(Nil, List(StrTypeStx), NullableTypeStx(IntTypeStx))))

    assert.complete(
      "[A, B, C](A?, B?) -> C?",
      FuncTypeStx(List("A", "B", "C"), List(NullableTypeStx(RefTypeStx("A")), NullableTypeStx(RefTypeStx("B"))), NullableTypeStx(RefTypeStx("C"))))

    assert.complete(
      "(A? | B?)",
      ParenTypeStx(UnionTypeStx(NullableTypeStx(RefTypeStx("A")), NullableTypeStx(RefTypeStx("B")))))

    assert.complete(
      "(Foo | Bar)?",
      NullableTypeStx(ParenTypeStx(UnionTypeStx(RefTypeStx("Foo"), RefTypeStx("Bar")))))
  }
}

object ExprParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  import PrefixOp._
  import InfixOp._

  object assert extends Assertions(expr)

  test("null") {
    assert.complete("null", NullExprStx)
  }

  test("boolean") {
    assert.complete("true", BoolExprStx(true))
    assert.complete("false", BoolExprStx(false))
  }

  test("number") {
    assert.complete("123", IntExprStx(123))
    assert.complete("123.456", DblExprStx(123.456))
    assert.partial("123 . 456", IntExprStx(123), 3)
  }

  test("string") {
    assert.complete("'dave'", StrExprStx("dave"))
    assert.complete("\"dave\"", StrExprStx("dave"))
    assert.complete("'\"dave\"'", StrExprStx("\"dave\""))
    assert.complete("\"'dave'\"", StrExprStx("'dave'"))
  }

  test("array") {
    assert.complete(
      "[ 1, 2 + 3, 4 ]",
      ArrExprStx(List(IntExprStx(1), InfixExprStx(Add, IntExprStx(2), IntExprStx(3)), IntExprStx(4))))

    assert.complete(
      "[1,2+3,4]",
      ArrExprStx(List(IntExprStx(1), InfixExprStx(Add, IntExprStx(2), IntExprStx(3)), IntExprStx(4))))

    assert.complete(
      "[ null , [ true && false ] , false ]",
      ArrExprStx(List(NullExprStx, ArrExprStx(List(InfixExprStx(And, BoolExprStx(true), BoolExprStx(false)))), BoolExprStx(false))))

    assert.complete(
      "[null,[true&&false],false]",
      ArrExprStx(List(NullExprStx, ArrExprStx(List(InfixExprStx(And, BoolExprStx(true), BoolExprStx(false)))), BoolExprStx(false))))
  }

  test("object") {
    assert.complete(
      "{ foo : null , \"'bar'\" : 1 + 2 , baz : true && false}",
      ObjExprStx(List(
        "foo" -> NullExprStx,
        "'bar'" -> InfixExprStx(Add, IntExprStx(1), IntExprStx(2)),
        "baz" -> InfixExprStx(And, BoolExprStx(true), BoolExprStx(false)))))

    assert.complete(
      "{foo:null,\"'bar'\":1+2,baz:true&&false}",
      ObjExprStx(List(
        "foo" -> NullExprStx,
        "'bar'" -> InfixExprStx(Add, IntExprStx(1), IntExprStx(2)),
        "baz" -> InfixExprStx(And, BoolExprStx(true), BoolExprStx(false)))))
  }

  test("ref") {
    assert.complete("i", RefExprStx("i"))
    assert.failure("if", 0)
    assert.complete("iff", RefExprStx("iff"))
  }

  test("cond") {
    assert.complete(
      "if a then b else c",
      CondExprStx(
        RefExprStx("a"),
        RefExprStx("b"),
        RefExprStx("c")))

    assert.complete(
      "ifathenbelsec",
      RefExprStx("ifathenbelsec"))

    assert.complete(
      "if(a)then(b)else(c)",
      CondExprStx(
        ParenExprStx(RefExprStx("a")),
        ParenExprStx(RefExprStx("b")),
        ParenExprStx(RefExprStx("c"))))

    assert.complete(
      "if a > b then c + d else e + f",
      CondExprStx(
        InfixExprStx(Gt, RefExprStx("a"), RefExprStx("b")),
        InfixExprStx(Add, RefExprStx("c"), RefExprStx("d")),
        InfixExprStx(Add, RefExprStx("e"), RefExprStx("f"))))
  }

  test("cast") {
    assert.complete(
      "123 : Int",
      CastExprStx(IntExprStx(123), IntTypeStx))

    assert.complete(
      "123 + 234: Int",
      InfixExprStx(
        InfixOp.Add,
        IntExprStx(123),
        CastExprStx(IntExprStx(234), IntTypeStx)))

    assert.complete(
      "(123 + 234): Int",
      CastExprStx(
        ParenExprStx(InfixExprStx(
          InfixOp.Add,
          IntExprStx(123),
          IntExprStx(234))),
        IntTypeStx))

    assert.complete(
      "123 : Int | String",
      CastExprStx(
        IntExprStx(123),
        UnionTypeStx(IntTypeStx, StrTypeStx)))

    assert.complete(
      "123 : (Int | String)",
      CastExprStx(
        IntExprStx(123),
        ParenTypeStx(UnionTypeStx(IntTypeStx, StrTypeStx))))
  }

  test("call") {
    assert.complete(
      "add ( a , b , c )",
      AppExprStx(RefExprStx("add"), List(RefExprStx("a"), RefExprStx("b"), RefExprStx("c"))))

    assert.complete(
      "add(a,b,c)",
      AppExprStx(RefExprStx("add"), List(RefExprStx("a"), RefExprStx("b"), RefExprStx("c"))))
  }

  test("paren") {
    assert.complete("( a )", ParenExprStx(RefExprStx("a")))
    assert.complete("(a)", ParenExprStx(RefExprStx("a")))
  }

  test("prefix") {
    assert.complete("- a", PrefixExprStx(Neg, RefExprStx("a")))
    assert.complete("+a", PrefixExprStx(Pos, RefExprStx("a")))
    assert.complete("!a", PrefixExprStx(Not, RefExprStx("a")))

    assert.complete(
      "+ a + + b",
      InfixExprStx(
        Add,
        PrefixExprStx(Pos, RefExprStx("a")),
        PrefixExprStx(Pos, RefExprStx("b"))))
  }

  test("infix") {
    assert.complete("a || b", InfixExprStx(Or, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a && b", InfixExprStx(And, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a == b", InfixExprStx(Eq, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a != b", InfixExprStx(Ne, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a > b", InfixExprStx(Gt, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a < b", InfixExprStx(Lt, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a >= b", InfixExprStx(Gte, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a <= b", InfixExprStx(Lte, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a + b", InfixExprStx(Add, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a - b", InfixExprStx(Sub, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a * b", InfixExprStx(Mul, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a / b", InfixExprStx(Div, RefExprStx("a"), RefExprStx("b")))

    assert.complete("a||b", InfixExprStx(Or, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a&&b", InfixExprStx(And, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a==b", InfixExprStx(Eq, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a!=b", InfixExprStx(Ne, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a>b", InfixExprStx(Gt, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a<b", InfixExprStx(Lt, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a>=b", InfixExprStx(Gte, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a<=b", InfixExprStx(Lte, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a+b", InfixExprStx(Add, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a-b", InfixExprStx(Sub, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a*b", InfixExprStx(Mul, RefExprStx("a"), RefExprStx("b")))
    assert.complete("a/b", InfixExprStx(Div, RefExprStx("a"), RefExprStx("b")))

    assert.complete(
      "a + b + c",
      InfixExprStx(
        Add,
        InfixExprStx(
          Add,
          RefExprStx("a"),
          RefExprStx("b")),
        RefExprStx("c")))

    assert.complete(
      "a * b + c",
      InfixExprStx(
        Add,
        InfixExprStx(
          Mul,
          RefExprStx("a"),
          RefExprStx("b")),
        RefExprStx("c")))

    assert.complete(
      "a + b * c",
      InfixExprStx(
        Add,
        RefExprStx("a"),
        InfixExprStx(
          Mul,
          RefExprStx("b"),
          RefExprStx("c"))))

    assert.complete(
      "( a + b ) * c",
      InfixExprStx(
        Mul,
        ParenExprStx(InfixExprStx(
          Add,
          RefExprStx("a"),
          RefExprStx("b"))),
        RefExprStx("c")))

    assert.complete(
      "a * (b + c)",
      InfixExprStx(
        Mul,
        RefExprStx("a"),
        ParenExprStx(InfixExprStx(
          Add,
          RefExprStx("b"),
          RefExprStx("c")))))

    assert.complete(
      "a <= b && c >= d",
      InfixExprStx(
        And,
        InfixExprStx(
          Lte,
          RefExprStx("a"),
          RefExprStx("b")),
        InfixExprStx(
          Gte,
          RefExprStx("c"),
          RefExprStx("d"))))

    assert.complete(
      "(a) + (b)",
      InfixExprStx(
        Add,
        ParenExprStx(RefExprStx("a")),
        ParenExprStx(RefExprStx("b"))))

    assert.complete(
      "+a + +b",
      InfixExprStx(
        Add,
        PrefixExprStx(Pos, RefExprStx("a")),
        PrefixExprStx(Pos, RefExprStx("b"))))
  }

  test("select") {
    assert.complete(
      "a . b",
      SelectExprStx(RefExprStx("a"), "b"))

    assert.complete(
      "a.b.c",
      SelectExprStx(SelectExprStx(RefExprStx("a"), "b"), "c"))

    assert.complete(
      "a.b+c.d",
      InfixExprStx(
        Add,
        SelectExprStx(RefExprStx("a"), "b"),
        SelectExprStx(RefExprStx("c"), "d")))
  }

  test("block") {
    assert.complete(
      "do a end",
      BlockExprStx(Nil, RefExprStx("a")))

    assert.complete(
      "doaend",
      RefExprStx("doaend"))

    assert.complete(
      i"""do a
      b end
      """,
      BlockExprStx(
        List(ExprStmtStx(RefExprStx("a"))),
        RefExprStx("b")))

    assert.failure(
      "do let a = 1 end",
      16)

    assert.complete(
      i"""
      do
        a
        b
        c
      end
      """,
      BlockExprStx(
        List(
          ExprStmtStx(RefExprStx("a")),
          ExprStmtStx(RefExprStx("b"))),
        RefExprStx("c")))

    assert.complete(
      "do;a;b;c;end",
      BlockExprStx(
        List(
          ExprStmtStx(RefExprStx("a")),
          ExprStmtStx(RefExprStx("b"))),
        RefExprStx("c")))

    assert.failure("do a b c end", 0)
  }

  test("func") {
    assert.complete(
      "( a, b ) -> a + b",
      FuncExprStx(
        Nil,
        List(FuncArgStx("a", None), FuncArgStx("b", None)),
        None,
        InfixExprStx(Add, RefExprStx("a"), RefExprStx("b"))))
    assert.complete(
      "( a , b : String ) : Int -> a + b",
      FuncExprStx(
        Nil,
        List(FuncArgStx("a", None), FuncArgStx("b", Some(StrTypeStx))),
        Some(IntTypeStx),
        InfixExprStx(Add, RefExprStx("a"), RefExprStx("b"))))

    assert.complete(
      "(a:Int,b):Real->a+b",
      FuncExprStx(
        Nil,
        List(FuncArgStx("a", Some(IntTypeStx)), FuncArgStx("b", None)),
        Some(DblTypeStx),
        InfixExprStx(Add, RefExprStx("a"), RefExprStx("b"))))

    assert.complete(
      "a -> b -> a + b",
      FuncExprStx(
        Nil,
        List(FuncArgStx("a", None)),
        None,
        FuncExprStx(
          Nil,
          List(FuncArgStx("b", None)),
          None,
          InfixExprStx(Add, RefExprStx("a"), RefExprStx("b")))))

    assert.complete(
      "a -> b -> a.c + b.d",
      FuncExprStx(
        Nil,
        List(FuncArgStx("a", None)),
        None,
        FuncExprStx(
          Nil,
          List(FuncArgStx("b", None)),
          None,
          InfixExprStx(Add,
            SelectExprStx(RefExprStx("a"), "c"),
            SelectExprStx(RefExprStx("b"), "d")))))
  }
}

object StmtParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  object assert extends Assertions(stmt)

  test("let") {
    assert.complete("let a = b", LetStmtStx("a", None, RefExprStx("b")))

    assert.complete("let a = b -> c", LetStmtStx(
      "a",
      None,
      FuncExprStx(
        Nil,
        List(FuncArgStx("b", None)),
        None,
        RefExprStx("c"))))

    assert.complete("let a: Int = b", LetStmtStx(
      "a",
      Some(IntTypeStx),
      RefExprStx("b")))

    assert.complete(
      i"""
      let add = ( a, b ) -> a + b
      """,
      LetStmtStx(
        "add",
        None,
        FuncExprStx(
          Nil,
          List(FuncArgStx("a", None), FuncArgStx("b", None)),
          None,
          InfixExprStx(InfixOp.Add, RefExprStx("a"), RefExprStx("b")))))
  }

  test("expr") {
    assert.complete(
      "a + b",
      ExprStmtStx(InfixExprStx(InfixOp.Add, RefExprStx("a"), RefExprStx("b"))))
  }
}

trait ParserSuiteHelpers {
  self: SimpleTestSuite =>

  class Assertions[A](parser: P[A]) {
    def complete(input: String, expected: A): Unit =
      partial(input, expected, input.length)

    def partial(input: String, expected: A, index: Int): Unit =
      parser.parse(input) match {
        case Parsed.Success(actual, n) =>
          assertEquals(actual, expected)
          assertEquals(n, index)
        case Parsed.Failure(_, _, _) =>
          fail(s"Could not parse input: [$input]")
      }

    def failure(input: String, index: Int): Unit =
      parser.parse(input) match {
        case Parsed.Success(value, _) =>
          fail(s"Expected parsing to fail: $input => $value")
        case Parsed.Failure(_, i, _) =>
          assertEquals(i, index)
      }
  }
}