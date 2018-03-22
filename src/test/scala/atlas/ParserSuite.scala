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

object ExprParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  import PrefixOp._
  import InfixOp._
  import Ast._
  import Ast.Literal._

  object assert extends Assertions(expr)

  test("null") {
    assert.complete("null", Null)
  }

  test("boolean") {
    assert.complete("true", True)
    assert.complete("false", False)
  }

  test("number") {
    assert.complete("123", Intr(123))
    assert.complete("123.456", Real(123.456))
    assert.partial("123 . 456", Intr(123), 3)
  }

  test("string") {
    assert.complete("'dave'", Str("dave"))
    assert.complete("\"dave\"", Str("dave"))
    assert.complete("'\"dave\"'", Str("\"dave\""))
    assert.complete("\"'dave'\"", Str("'dave'"))
  }

  test("array") {
    assert.complete(
      "[ 1, 2 + 3, 4 ]",
      Arr(List(Intr(1), Infix(Add, Intr(2), Intr(3)), Intr(4))))

    assert.complete(
      "[1,2+3,4]",
      Arr(List(Intr(1), Infix(Add, Intr(2), Intr(3)), Intr(4))))

    assert.complete(
      "[ null , [ true && false ] , false ]",
      Arr(List(Null, Arr(List(Infix(And, True, False))), False)))

    assert.complete(
      "[null,[true&&false],false]",
      Arr(List(Null, Arr(List(Infix(And, True, False))), False)))
  }

  test("object") {
    assert.complete(
      "{ foo : null , \"'bar'\" : 1 + 2 , baz : true && false}",
      Obj(List(
        "foo" -> Null,
        "'bar'" -> Infix(Add, Intr(1), Intr(2)),
        "baz" -> Infix(And, True, False))))

    assert.complete(
      "{foo:null,\"'bar'\":1+2,baz:true&&false}",
      Obj(List(
        "foo" -> Null,
        "'bar'" -> Infix(Add, Intr(1), Intr(2)),
        "baz" -> Infix(And, True, False))))
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
      "ifathenbelsec",
      Ref("ifathenbelsec"))

    assert.complete(
      "if(a)then(b)else(c)",
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

    assert.complete(
      "add(a,b,c)",
      Apply(Ref("add"), List(Ref("a"), Ref("b"), Ref("c"))))
  }

  test("paren") {
    assert.complete("( a )", Ref("a"))
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

    assert.complete("a||b", Infix(Or, Ref("a"), Ref("b")))
    assert.complete("a&&b", Infix(And, Ref("a"), Ref("b")))
    assert.complete("a==b", Infix(Eq, Ref("a"), Ref("b")))
    assert.complete("a!=b", Infix(Ne, Ref("a"), Ref("b")))
    assert.complete("a>b", Infix(Gt, Ref("a"), Ref("b")))
    assert.complete("a<b", Infix(Lt, Ref("a"), Ref("b")))
    assert.complete("a>=b", Infix(Gte, Ref("a"), Ref("b")))
    assert.complete("a<=b", Infix(Lte, Ref("a"), Ref("b")))
    assert.complete("a+b", Infix(Add, Ref("a"), Ref("b")))
    assert.complete("a-b", Infix(Sub, Ref("a"), Ref("b")))
    assert.complete("a*b", Infix(Mul, Ref("a"), Ref("b")))
    assert.complete("a/b", Infix(Div, Ref("a"), Ref("b")))

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

  test("cast") {
    assert.complete(
      "a : Int",
      Cast(Ref("a"), Type.Intr))

    assert.complete(
      "a:Int",
      Cast(Ref("a"), Type.Intr))

    assert.complete(
      "-a : Int",
      Cast(Prefix(PrefixOp.Neg, Ref("a")), Type.Intr))

    assert.complete(
      "a + b : Int",
      Infix(InfixOp.Add, Ref("a"), Cast(Ref("b"), Type.Intr)))

    assert.complete(
      "(a + b) : Int",
      Cast(Infix(InfixOp.Add, Ref("a"), Ref("b")), Type.Intr))

    assert.complete(
      "a.b : Int",
      Cast(Select(Ref("a"), "b"), Type.Intr))

    assert.complete(
      "(do a end) : Int",
      Cast(Block(Nil, Ref("a")), Type.Intr))

    assert.complete(
      "if a then b else c : Int",
      Cond(Ref("a"), Ref("b"), Cast(Ref("c"), Type.Intr)))

    assert.complete(
      "(if a then b else c) : Int",
      Cast(Cond(Ref("a"), Ref("b"), Ref("c")), Type.Intr))

    assert.partial(
      "do a end : Int",
      Block(Nil, Ref("a")),
      8)
  }

  test("select") {
    assert.complete(
      "a . b",
      Select(Ref("a"), "b"))

    assert.complete(
      "a.b.c",
      Select(Select(Ref("a"), "b"), "c"))

    assert.complete(
      "a.b+c.d",
      Infix(
        Add,
        Select(Ref("a"), "b"),
        Select(Ref("c"), "d")))
  }

  test("block") {
    assert.complete(
      "do a end",
      Block(Nil, Ref("a")))

    assert.complete(
      "doaend",
      Ref("doaend"))

    assert.complete(
      i"""do a
      b end
      """,
      Block(
        List(ExprStmt(Ref("a"))),
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
        List(
          ExprStmt(Ref("a")),
          ExprStmt(Ref("b"))),
        Ref("c")))

    assert.complete(
      "do;a;b;c;end",
      Block(
        List(
          ExprStmt(Ref("a")),
          ExprStmt(Ref("b"))),
        Ref("c")))

    assert.failure("do a b c end", 3)
  }

  test("func") {
    assert.complete(
      "( a, b ) -> a + b",
      Func(
        List(Arg("a"), Arg("b")),
        None,
        Infix(Add, Ref("a"), Ref("b"))))

    assert.complete(
      "( a , b : String ) : Int -> a + b",
      Func(
        List(Arg("a"), Arg("b", Some(Type.Str))),
        Some(Type.Intr),
        Infix(Add, Ref("a"), Ref("b"))))

    assert.complete(
      "(a:Int,b):Real->a+b",
      Func(
        List(Arg("a", Some(Type.Intr)), Arg("b")),
        Some(Type.Real),
        Infix(Add, Ref("a"), Ref("b"))))

    assert.complete(
      "a -> b -> a + b",
      Func(
        List(Arg("a")),
        None,
        Func(
          List(Arg("b")),
          None,
          Infix(Add, Ref("a"), Ref("b")))))

    assert.complete(
      "a -> b -> a.c + b.d",
      Func(
        List(Arg("a")),
        None,
        Func(
          List(Arg("b")),
          None,
          Infix(Add,
            Select(Ref("a"), "c"),
            Select(Ref("b"), "d")))))
  }
}

object StmtParserSuite extends SimpleTestSuite with AllParsers with ParserSuiteHelpers {
  import Ast._
  import Ast.Literal._
  import InfixOp._

  object assert extends Assertions(stmt)

  test("defn") {
    assert.complete("let a = b", DefnStmt("a", None, Ref("b")))

    assert.complete("let a = b -> c", DefnStmt(
      "a",
      None,
      Func(
        List(Arg("b")),
        None,
        Ref("c"))))

    assert.complete("let a: Int = b", DefnStmt(
      "a",
      Some(Type.Intr),
      Ref("b")))

    assert.complete(
      i"""
      let add = ( a, b ) -> a + b
      """,
      DefnStmt(
        "add",
        None,
        Func(
          List(Arg("a"), Arg("b")),
          None,
          Infix(Add, Ref("a"), Ref("b")))))
  }

  test("expr") {
    assert.complete(
      "a + b",
      ExprStmt(
        Infix(Add, Ref("a"), Ref("b"))))
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