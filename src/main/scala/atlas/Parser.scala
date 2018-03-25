package atlas

import fastparse.all._
import fastparse.parsers.Combinators.Rule

object Parser {
  object parsers extends AllParsers

  case class Error(failure: Parsed.Failure)

  def expr(code: String): Either[Error, Expr] =
    parse(parsers.exprToEnd, code)

  def prog(code: String): Either[Error, Expr] =
    parse(parsers.progToEnd, code)

  private def parse[A](parser: Parser[A], code: String): Either[Error, A] = {
    parser.parse(code) match {
      case failure: Parsed.Failure =>
        Left(Error(failure))

      case Parsed.Success(value, _) =>
        Right(value)
    }
  }
}

trait AllParsers {
  val alpha: Parser[Unit] =
    P(CharIn("_$", 'a' to 'z', 'A' to 'Z'))

  val digit: Parser[Unit] =
    P(CharIn('0' to '9'))

  val hexDigit: Parser[Unit] =
    P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val identStart: Parser[Unit] =
    P(alpha)

  val identCont: Parser[Unit] =
    P(identStart | digit)

  val newline: Parser[Unit] =
    P("\n" | "\r\n" | "\r" | "\f" | ";")

  val whitespace: Parser[Unit] =
    P(" " | "\t")

  val lineComment: Parser[Unit] =
    P("#" ~ CharsWhile(_ != '\n', min = 0) ~ &(newline | End))

  val comment: Parser[Unit] =
    P(lineComment)

  val escape: Parser[Unit] =
    P("\\" ~ ((!(newline | hexDigit) ~ AnyChar) | (hexDigit.rep(min = 1, max = 6) ~ whitespace.?)))

  val nl: Parser[Unit] =
    P(whitespace.rep ~ comment.? ~ newline ~ (whitespace | newline).rep)

  val ws: Parser[Unit] =
    P((whitespace | comment | newline).rep)

  val letKw: Parser[Unit] =
    P("let" ~ !identCont)

  val doKw: Parser[Unit] =
    P("do" ~ !identCont)

  val endKw: Parser[Unit] =
    P("end" ~ !identCont)

  val ifKw: Parser[Unit] =
    P("if" ~ !identCont)

  val thenKw: Parser[Unit] =
    P("then" ~ !identCont)

  val elseKw: Parser[Unit] =
    P("else" ~ !identCont)

  val falseKw: Parser[Unit] =
    P("false" ~ !identCont)

  val trueKw: Parser[Unit] =
    P("true" ~ !identCont)

  val nullKw: Parser[Unit] =
    P("null" ~ !identCont)

  val keyword: Parser[Unit] =
    P(letKw | doKw | endKw | ifKw | thenKw | elseKw | falseKw | trueKw | nullKw)

  val ident: Parser[String] =
    P(!keyword ~ identStart ~ identCont.rep).!

  // In increasing order of precedence:
  val infixOps: List[Parser[InfixOp]] = {
    import InfixOp._
    def op(id: String, op: InfixOp): Parser[InfixOp] =
      Rule(id, () => id).map(_ => op)
    List(
      op("||", Or),
      op("&&", And),
      op("==", Eq)  | op("!=", Ne),
      op("<=", Lte) | op(">=", Gte) | op("<", Lt) | op(">", Gt),
      op("+", Add)  | op("-", Sub),
      op("*", Mul)  | op("/", Div)
    )
  }

  val prefixOp: Parser[PrefixOp] = {
    import PrefixOp._
    def op(id: String, op: PrefixOp): Parser[PrefixOp] =
      Rule(id, () => id).map(_ => op)
    op("+", Pos) | op("-", Neg) | op("!", Not)
  }

  val booleanToken: Parser[String] =
    P((trueKw | falseKw).!)

  val intToken: Parser[String] =
    P((CharIn("+-").? ~ digit.rep(1)).!)

  val doubleToken: Parser[String] = {
    val sign = P(CharIn("+-"))
    val wholeOnly = P(digit.rep(1))
    val wholeAndFraction = P(digit.rep(1) ~ "." ~/ digit.rep(0))
    val fractionOnly = P("." ~/ digit.rep(1))
    val exponent = P(CharIn("eE") ~/ sign.? ~ digit.rep(1))
    P((sign.? ~ (fractionOnly ~ exponent.? | wholeAndFraction ~ exponent.? | wholeOnly ~ exponent)).!)
  }

  val stringToken: Parser[String] = {
    val newline = P("\n" | "\r\n" | "\r" | "\f")
    val escape =
      P("\\" ~ ((!(newline | hexDigit) ~ AnyChar) | (hexDigit.rep(min = 1, max = 6) ~ whitespace.?)))
    def unescaped(quote: String) =
      P((!(quote | "\\" | newline) ~ AnyChar) | escape | ("\\" ~ newline))
    def unescape(value: String) =
      org.apache.commons.lang3.StringEscapeUtils.unescapeJava(value)
    def quoted(quote: String) =
      P(quote ~ (escape | unescaped(quote)).rep.!.map(unescape) ~ quote)
    val doubleQuoted = quoted("\'")
    val singleQuoted = quoted("\"")
    P(doubleQuoted | singleQuoted)
  }

  def ref(p: Parser[Unit]): Parser[RefExpr] =
    P(p.!.map(RefExpr))

  // -----

  def toEnd[A](parser: Parser[A]): Parser[A] =
    P(ws ~ parser ~ ws ~ End)

  val exprToEnd: Parser[Expr] =
    P(toEnd(expr))

  val progToEnd: Parser[Expr] =
    P(toEnd(prog))

  // -----

  val prog: Parser[Expr] =
    P(stmt.rep(min = 1, sep = nl)).flatMap(validateBlock)

  // -----

  val stmt: Parser[Expr] =
    P(let | expr)

  // -----

  val let: Parser[Expr] =
    P(letKw ~ ws ~/ ident ~ ws ~ "=" ~ ws ~/ expr)
      .map { case (name, expr) => LetExpr(name, expr) }

  // -----

  val expr: Parser[Expr] =
    P(block)

  // -----

  def validateBlock(stmts: Seq[Expr]): Parser[Expr] =
    (stmts.init, stmts.last) match {
      case (stmts, expr: LetExpr) => Fail // BlockExpr(stmts, expr)
      case (stmts, expr)          => Pass.map(_ => BlockExpr(stmts.toList, expr))
    }

  val blockHit: Parser[Expr] =
    P(doKw ~ ws ~ stmt.rep(min = 1, sep = nl) ~ ws ~ endKw)
      .flatMap(validateBlock)

  val block: Parser[Expr] =
    P(blockHit | cond)

  // -----

  val condHit: Parser[Expr] =
    P(ifKw ~ ws ~ expr ~ ws ~ thenKw ~ ws ~ expr ~ ws ~ elseKw ~ ws ~ expr)
      .map { case (test, ifTrue, ifFalse) => CondExpr(test, ifTrue, ifFalse) }

  val cond: Parser[Expr] =
    P(condHit | infix)

  // -----

  def createInfix(op: Parser[InfixOp], subExpr: Parser[Expr]): Parser[Expr] =
    P(subExpr ~ (ws ~ op ~ ws ~ subExpr).rep).map {
      case (head, tail) =>
        tail.foldLeft(head) { (a, pair) =>
          val (op, b) = pair
          InfixExpr(op, a, b)
        }
    }

  val infix: Parser[Expr] =
    P(infixOps.foldRight(prefix)(createInfix))

  // -----

  val prefixHit: Parser[Expr] =
    P(prefixOp ~ ws ~ prefix)
      .map { case (op, arg) => PrefixExpr(op, arg) }

  val prefix: Parser[Expr] =
    P(prefixHit | select)

  // -----

  val select: Parser[Expr] =
    P(apply ~ (ws ~ "." ~ ws ~ ident).rep)
      .map { case (head, tail) => tail.foldLeft(head)(SelectExpr) }

  // -----

  val applyHit: Parser[Expr] =
    P(ident ~ ws ~ "(" ~/ ws ~ expr.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")")
      .map { case (name, args) => AppExpr(RefExpr(name), args.toList) }

  val apply: Parser[Expr] =
    P(applyHit | func)

  // -----

  val arg: Parser[FuncArg] =
    P(ident).map(FuncArg)

  val parenFuncHit: Parser[Expr] =
    P("(" ~ ws ~ arg.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")" ~ ws ~ "->" ~ ws ~/ expr)
      .map { case (args, expr) => FuncExpr(args.toList, expr) }

  val noParenFuncHit: Parser[Expr] =
    P(ident ~ ws ~ "->" ~ ws ~/ expr)
      .map { case (name, expr) => FuncExpr(List(FuncArg(name)), expr) }

  val func: Parser[Expr] =
    P(parenFuncHit | noParenFuncHit | obj)

  // -----

  val field: Parser[(String, Expr)] =
    P((ident | stringToken) ~ ws ~/ ":" ~ ws ~/ expr)

  val objHit: Parser[Expr] =
    P("{" ~ ws ~/ field.rep(sep = ws ~ "," ~ ws.~/) ~ ws ~ "}".~/)
      .map(_.toList)
      .map(ObjExpr)

  val obj: Parser[Expr] =
    P(objHit | arr)

  // -----

  val arrHit: Parser[Expr] =
    P("[" ~ ws ~/ expr.rep(sep = ws ~ "," ~/ ws) ~ ws ~ "]".~/)
      .map(_.toList)
      .map(ArrExpr)

  val arr: Parser[Expr] =
    P(arrHit | paren)

  // -----

  val parenHit: Parser[Expr] =
    P("(" ~ ws ~ expr ~ ws ~ ")")

  val paren: Parser[Expr] =
    P(parenHit | atom)

  // -----

  val atom: Parser[Expr] =
    P(str | double | int | trueExpr | falseExpr | nullExpr | ref)

  val str: Parser[Expr] =
    P(stringToken).map(StrExpr)

  val double: Parser[Expr] =
    P(doubleToken).map(_.toDouble).map(DblExpr)

  val int: Parser[Expr] =
    P(intToken).map(_.toInt).map(IntExpr)

  val trueExpr: Parser[Expr] =
    P(trueKw).map(_ => BoolExpr(true))

  val falseExpr: Parser[Expr] =
    P(falseKw).map(_ => BoolExpr(false))

  val nullExpr: Parser[Expr] =
    P(nullKw).map(_ => NullExpr)

  val ref: Parser[Expr] =
    P(ident.map(RefExpr))
}
