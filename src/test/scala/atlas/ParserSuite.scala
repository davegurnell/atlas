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
    assert.partial("truefalse", "true", 4)
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
    assert.failure("if", 0)
    assert.complete("ifdave", "ifdave")
    assert.partial("dave was here", "dave", 4)
  }
}

object ExprParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  import PrefixOp._
  import InfixOp._
  import Ast._

  object assert extends Assertions(expr)

  test("null") {
    assert.complete("null", NullLiteral)
  }

  test("boolean") {
    assert.complete("true", TrueLiteral)
    assert.complete("false", FalseLiteral)
  }

  test("number") {
    assert.complete("123", IntLiteral(123))
    assert.complete("123.456", DoubleLiteral(123.456))
  }

  test("string") {
    assert.complete("'dave'", StringLiteral("dave"))
    assert.complete("\"dave\"", StringLiteral("dave"))
    assert.complete("'\"dave\"'", StringLiteral("\"dave\""))
    assert.complete("\"'dave'\"", StringLiteral("'dave'"))
  }

  test("array") {
    assert.complete(
      "[ 1, 2 + 3, 4 ]",
      ArrayLiteral(List(IntLiteral(1), Infix(Add, IntLiteral(2), IntLiteral(3)), IntLiteral(4))))

    assert.complete(
      "[ null , [ true && false ] , false ]",
      ArrayLiteral(List(NullLiteral, ArrayLiteral(List(Infix(And, TrueLiteral, FalseLiteral))), FalseLiteral)))
  }

  test("object") {
    assert.complete(
      "{ foo : null , \"'bar'\" : 1 + 2 , baz : true && false}",
      ObjectLiteral(List(
        "foo" -> NullLiteral,
        "'bar'" -> Infix(Add, IntLiteral(1), IntLiteral(2)),
        "baz" -> Infix(And, TrueLiteral, FalseLiteral))))

    assert.complete(
      "[ null , [ true ] , false ]",
      ArrayLiteral(List(NullLiteral, ArrayLiteral(List(TrueLiteral)), FalseLiteral)))
  }

  test("ref") {
    assert.complete("i", Ref("i"))
    assert.failure("if", 0)
    assert.complete("iff", Ref("iff"))
  }

  test("cond") {
    assert.complete(
      "if a then b else c",
      Cond(
        Ref("a"),
        Ref("b"),
        Ref("c")))

    assert.complete(
      "if a > b then c + d else e + f",
      Cond(
        Infix(Gt, Ref("a"), Ref("b")),
        Infix(Add, Ref("c"), Ref("d")),
        Infix(Add, Ref("e"), Ref("f"))))
  }

  test("call") {
    assert.complete(
      "add ( a , b , c )",
      Apply(Ref("add"), List(Ref("a"), Ref("b"), Ref("c"))))
  }

  test("paren") {
    assert.complete("(a)", Ref("a"))
  }

  test("prefix") {
    assert.complete("- a", Prefix(Neg, Ref("a")))
    assert.complete("+a", Prefix(Pos, Ref("a")))
    assert.complete("!a", Prefix(Not, Ref("a")))

    assert.complete(
      "+ a + + b",
      Infix(
        Add,
        Prefix(Pos, Ref("a")),
        Prefix(Pos, Ref("b"))))
  }

  test("infix") {
    assert.complete("a || b", Infix(Or, Ref("a"), Ref("b")))
    assert.complete("a && b", Infix(And, Ref("a"), Ref("b")))
    assert.complete("a == b", Infix(Eq, Ref("a"), Ref("b")))
    assert.complete("a != b", Infix(Ne, Ref("a"), Ref("b")))
    assert.complete("a > b", Infix(Gt, Ref("a"), Ref("b")))
    assert.complete("a < b", Infix(Lt, Ref("a"), Ref("b")))
    assert.complete("a >= b", Infix(Gte, Ref("a"), Ref("b")))
    assert.complete("a <= b", Infix(Lte, Ref("a"), Ref("b")))
    assert.complete("a + b", Infix(Add, Ref("a"), Ref("b")))
    assert.complete("a - b", Infix(Sub, Ref("a"), Ref("b")))
    assert.complete("a * b", Infix(Mul, Ref("a"), Ref("b")))
    assert.complete("a / b", Infix(Div, Ref("a"), Ref("b")))

    assert.complete(
      "a + b + c",
      Infix(
        Add,
        Infix(
          Add,
          Ref("a"),
          Ref("b")),
        Ref("c")))

    assert.complete(
      "a * b + c",
      Infix(
        Add,
        Infix(
          Mul,
          Ref("a"),
          Ref("b")),
        Ref("c")))

    assert.complete(
      "a + b * c",
      Infix(
        Add,
        Ref("a"),
        Infix(
          Mul,
          Ref("b"),
          Ref("c"))))

    assert.complete(
      "( a + b ) * c",
      Infix(
        Mul,
        Infix(
          Add,
          Ref("a"),
          Ref("b")),
        Ref("c")))

    assert.complete(
      "a * (b + c)",
      Infix(
        Mul,
        Ref("a"),
        Infix(
          Add,
          Ref("b"),
          Ref("c"))))

    assert.complete(
      "a <= b && c >= d",
      Infix(
        And,
        Infix(
          Lte,
          Ref("a"),
          Ref("b")),
        Infix(
          Gte,
          Ref("c"),
          Ref("d"))))

    assert.complete(
      "(a) + (b)",
      Infix(
        Add,
        Ref("a"),
        Ref("b")))

    assert.complete(
      "+a + +b",
      Infix(
        Add,
        Prefix(Pos, Ref("a")),
        Prefix(Pos, Ref("b"))))
  }

  test("select") {
    assert.complete(
      "a . b",
      Select(Ref("a"), Ref("b")))

    assert.complete(
      "a.b.c",
      Select(
        Select(Ref("a"), Ref("b")),
        Ref("c")))

    assert.complete(
      "a.b + c.d",
      Infix(
        Add,
        Select(Ref("a"), Ref("b")),
        Select(Ref("c"), Ref("d"))))
  }

  test("block") {
    assert.complete(
      "do a end",
      Block(Nil, Ref("a")))

    assert.complete(
      i"""do a
      b end
      """,
      Block(
        List(Ref("a")),
        Ref("b")))

    assert.complete(
      i"""
      do
        a
        b
        c
      end
      """,
      Block(
        List(Ref("a"), Ref("b")),
        Ref("c")))

    assert.complete(
      "do;a;b;c;end",
      Block(
        List(Ref("a"), Ref("b")),
        Ref("c")))

    assert.failure("do a b c end", 3)
  }

  test("func") {
    assert.complete(
      "( a, b ) -> a + b",
      FuncLiteral(
        List(Ref("a"), Ref("b")),
        Infix(Add, Ref("a"), Ref("b"))))

    assert.complete(
      "a -> b -> a + b",
      FuncLiteral(
        List(Ref("a")),
        FuncLiteral(
          List(Ref("b")),
          Infix(Add, Ref("a"), Ref("b")))))

    assert.complete(
      "a -> b -> a.c + b.d",
      FuncLiteral(
        List(Ref("a")),
        FuncLiteral(
          List(Ref("b")),
          Infix(Add,
            Select(Ref("a"), Ref("c")),
            Select(Ref("b"), Ref("d"))))))
  }
}

object StmtParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  import Ast._
  import PrefixOp._
  import InfixOp._

  object assert extends Assertions(stmt)

  test("defn") {
    assert.complete("let a = b", Defn(Ref("a"), Ref("b")))

    assert.complete("let a = b -> c", Defn(
      Ref("a"),
      FuncLiteral(List(Ref("b")), Ref("c"))))

    assert.complete(
      i"""
      let add = ( a, b ) -> a + b
      """,
      Defn(
        Ref("add"),
        FuncLiteral(
          List(Ref("a"), Ref("b")),
          Infix(Add, Ref("a"), Ref("b")))))
  }

  test("expr") {
    assert.complete("a + b", Infix(Add, Ref("a"), Ref("b")))
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