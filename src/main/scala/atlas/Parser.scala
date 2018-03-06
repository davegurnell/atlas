package atlas

import fastparse.all._

object Parser {
  import Ast._

  object parsers extends AllParsers

  case class Error(failure: Parsed.Failure)

  def expr(code: String): Either[Error, Expr] =
    parse(parsers.exprToEnd, code)

  def stmt(code: String): Either[Error, Stmt] =
    parse(parsers.stmtToEnd, code)

  def prog(code: String): Either[Error, Block] =
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
  import Ast._

  val letKw = "let"
  val doKw = "do"
  val endKw = "end"
  val ifKw = "if"
  val thenKw = "then"
  val elseKw = "else"
  val falseKw = "false"
  val trueKw = "true"
  val nullKw = "null"

  val kws = List(letKw, doKw, endKw, ifKw, thenKw, elseKw, falseKw, trueKw, nullKw)

  val digit: Parser[Unit] =
    P(CharIn('0' to '9'))

  val hexDigit: Parser[Unit] =
    P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val identStart: Parser[Unit] =
    P(CharIn("_$", '0' to '9', 'a' to 'z', 'A' to 'Z'))

  val identCont: Parser[Unit] =
    P(identStart | digit)

  val newline: Parser[Unit] =
    P("\n" | "\r\n" | "\r" | "\f" | ";")

  val whitespace: Parser[Unit] =
    P(" " | "\t")

  val nl: Parser[Unit] =
    P(whitespace.rep ~ newline ~ (whitespace | newline).rep)

  val ws: Parser[Unit] =
    P((whitespace | newline).rep)

  val comment: Parser[Unit] =
    P("#" ~ CharsWhile(_ != '\n', min = 0))

  val escape: Parser[Unit] =
    P("\\" ~ ((!(newline | hexDigit) ~ AnyChar) | (hexDigit.rep(min = 1, max = 6) ~ whitespace.?)))

  val keyword: Parser[Unit] =
    P(kws.map(kw => P(kw ~ !identCont)).reduceLeft(_ | _))

  // In increasing order of precedence:
  val infixOps: List[Parser[InfixOp]] = {
    import InfixOp._
    def op(id: String, op: InfixOp): Parser[InfixOp] =
      P(id).map(_ => op)
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
      P(id).map(_ => op)
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

  val ident: Parser[String] =
    P((identStart ~ identCont.rep ~ !identCont).!.filter(ident => !kws.contains(ident)))

  def ref(p: Parser[Unit]): Parser[Ref] =
    P(p.!.map(Ref))

  // -----

  def toEnd[A](parser: Parser[A]): Parser[A] =
    P(ws ~ parser ~ ws ~ End)

  val exprToEnd: Parser[Expr] =
    P(toEnd(expr))

  val stmtToEnd: Parser[Stmt] =
    P(toEnd(stmt))

  val progToEnd: Parser[Block] =
    P(toEnd(prog))

  // -----

  val prog: Parser[Block] =
    P(stmt.rep(sep = nl)).map { stmts =>
      stmts.last match {
        case expr: Expr => Block(stmts.init.toList, expr)
        case defn: Defn => Block(stmts.toList, NullLiteral)
      }
    }

  // -----

  val stmt: Parser[Stmt] =
    P(defn | expr)

  // -----

  val defn: Parser[Defn] =
    P(letKw ~ ws ~/ ref ~ ws ~ "=" ~ ws ~/ expr)
      .map { case (ref, expr) => Defn(ref, expr) }

  // -----

  val expr: Parser[Expr] =
    P(block)

  // -----

  val blockBodyExpr: Parser[(Seq[Stmt], Expr)] =
    P(expr ~ ws ~ endKw.~/)
      .map(e => (Seq.empty[Stmt], e))

  val blockBodyStmt: Parser[(Seq[Stmt], Expr)] =
    P(stmt ~ nl ~ blockBody)
      .map { case (s, (ss, e)) => (s +: ss, e) }

  val blockBody: Parser[(Seq[Stmt], Expr)] =
    P(blockBodyExpr | blockBodyStmt)

  val blockHit: Parser[Expr] =
    P(doKw ~ ws ~/ blockBody)
      .map { case (stmts, expr) => Block(stmts.toList, expr) }

  val block: Parser[Expr] =
    P(blockHit | cond)

  // -----

  val condHit: Parser[Expr] =
    P(ifKw ~ ws ~ expr ~ ws ~ thenKw ~ ws ~ expr ~ ws ~ elseKw ~ ws ~ expr)
      .map { case (test, ifTrue, ifFalse) => Cond(test, ifTrue, ifFalse) }

  val cond: Parser[Expr] =
    P(condHit | infix)

  // -----

  def createInfix(op: Parser[InfixOp], subExpr: Parser[Expr]): Parser[Expr] =
    P(subExpr ~ (ws ~ op ~ ws ~ subExpr).rep).map {
      case (head, tail) =>
        tail.foldLeft(head) { (a, pair) =>
          val (op, b) = pair
          Infix(op, a, b)
        }
    }

  val infix: Parser[Expr] =
    P(infixOps.foldRight(prefix)(createInfix))

  // -----

  val prefixHit: Parser[Expr] =
    P(prefixOp ~ ws ~ prefix)
      .map { case (op, arg) => Prefix(op, arg) }

  val prefix: Parser[Expr] =
    P(prefixHit | select)

  // -----

  val select: Parser[Expr] =
    P(apply ~ (ws ~ "." ~ ws ~ ref).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Select) }

  // -----

  val applyHit: Parser[Expr] =
    P(ident ~ ws ~ "(" ~/ ws ~ expr.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")")
      .map { case (name, args) => Apply(Ref(name), args.toList) }

  val apply: Parser[Expr] =
    P(applyHit | func)

  // -----

  val parenFuncHit: Parser[Expr] =
    P("(" ~ ws ~ ref.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")" ~ ws ~ "->" ~ ws ~/ expr)
      .map { case (args, expr) => FuncLiteral(args.toList, expr) }

  val noParenFuncHit: Parser[Expr] =
    P(ref ~ ws ~ "->" ~ ws ~/ expr)
      .map { case (arg, expr) => FuncLiteral(List(arg), expr) }

  val func: Parser[Expr] =
    P(parenFuncHit | noParenFuncHit | obj)

  // -----

  val field: Parser[(String, Expr)] =
    P((ident | stringToken) ~ ws ~/ ":" ~ ws ~/ expr)

  val objHit: Parser[Expr] =
    P("{" ~ ws ~/ field.rep(sep = ws ~ "," ~ ws.~/) ~ ws ~ "}".~/)
      .map(_.toList)
      .map(ObjectLiteral)

  val obj: Parser[Expr] =
    P(objHit | arr)

  // -----

  val arrHit: Parser[Expr] =
    P("[" ~ ws ~/ expr.rep(sep = ws ~ "," ~/ ws) ~ ws ~ "]".~/)
      .map(_.toList)
      .map(ArrayLiteral)

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
    P(stringToken).map(StringLiteral)

  val double: Parser[Expr] =
    P(doubleToken).map(_.toDouble).map(DoubleLiteral)

  val int: Parser[Expr] =
    P(intToken).map(_.toInt).map(IntLiteral)

  val trueExpr: Parser[Expr] =
    P(trueKw).map(_ => TrueLiteral)

  val falseExpr: Parser[Expr] =
    P(falseKw).map(_ => FalseLiteral)

  val nullExpr: Parser[Expr] =
    P(nullKw).map(_ => NullLiteral)

  val ref: Parser[Ref] =
    P(ident.map(Ref))
}
