package atlas

import fastparse.all._
import fastparse.parsers.Combinators.Rule

object Parser {
  object parsers extends AllParsers

  case class Error(failure: Parsed.Failure)

  def expr(code: String): Either[Error, ExprStx] =
    parse(parsers.exprToEnd, code)

  def prog(code: String): Either[Error, ExprStx] =
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

  val letTypeKw: Parser[Unit] =
    P("type" ~ !identCont)

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
    P(letKw | letTypeKw | doKw | endKw | ifKw | thenKw | elseKw | falseKw | trueKw | nullKw)

  val ident: Parser[String] =
    P(!keyword ~ identStart ~ identCont.rep).!

  val intTypeKw: Parser[Unit] =
    P("Int" ~ !identCont)

  val dblTypeKw: Parser[Unit] =
    P("Real" ~ !identCont)

  val strTypeKw: Parser[Unit] =
    P("String" ~ !identCont)

  val boolTypeKw: Parser[Unit] =
    P("Boolean" ~ !identCont)

  val nullTypeKw: Parser[Unit] =
    P("Null" ~ !identCont)

  val typeKw: Parser[Unit] =
    P(intTypeKw | dblTypeKw | strTypeKw | boolTypeKw | nullTypeKw)

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

  def ref(p: Parser[Unit]): Parser[RefExprStx] =
    P(p.!.map(RefExprStx))

  // -----

  def toEnd[A](parser: Parser[A]): Parser[A] =
    P(ws ~ parser ~ ws ~ End)

  val exprToEnd: Parser[ExprStx] =
    P(toEnd(expr))

  val progToEnd: Parser[ExprStx] =
    P(toEnd(prog))

  // -----

  val prog: Parser[ExprStx] =
    P(stmt.rep(min = 1, sep = nl)).flatMap(validateBlock)

  // -----

  val tpe: Parser[TypeStx] =
    P(funcType)

  // -----

  val parenFuncTypeHit: Parser[TypeStx] =
    P(("[" ~ ws ~ ident.rep(sep = ws ~ "," ~ ws) ~ ws ~ "]").? ~ "(" ~ ws ~ tpe.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")" ~ ws ~ "->" ~ ws ~ tpe)
      .map { case (typeVars, argTypes, resType) => FuncTypeStx(typeVars.map(_.toList).getOrElse(Nil), argTypes.toList, resType) }

  val noParenFuncTypeHit: Parser[TypeStx] =
    P(atomicType ~ ws ~ "->" ~ ws ~ tpe)
      .map { case (argType, resType) => FuncTypeStx(Nil, List(argType), resType) }

  val funcType: Parser[TypeStx] =
    P(parenFuncTypeHit | noParenFuncTypeHit | unionType)

  // -----

  val unionType: Parser[TypeStx] =
    P(nullableType ~ (ws ~ "|" ~ ws ~ nullableType).rep).map {
      case (head, tail) =>
        tail.foldLeft(head)(UnionTypeStx)
    }

  // -----

  val nullableType: Parser[TypeStx] =
    P(parenType ~ (ws ~ "?".!).rep).map {
      case (tpe, Nil) => tpe
      case (tpe, qns) => NullableTypeStx(tpe)
    }

  // -----

  val parenTypeHit: Parser[TypeStx] =
    P("(" ~ ws ~ tpe ~ ws ~ ")").map(ParenTypeStx)

  val parenTypeMiss: Parser[TypeStx] =
    P(atomicType)

  val parenType: Parser[TypeStx] =
    P(parenTypeHit | parenTypeMiss)

  // -----

  val intType: Parser[TypeStx] =
    P(intTypeKw).map(_ => IntTypeStx)

  val dblType: Parser[TypeStx] =
    P(dblTypeKw).map(_ => DblTypeStx)

  val strType: Parser[TypeStx] =
    P(strTypeKw).map(_ => StrTypeStx)

  val boolType: Parser[TypeStx] =
    P(boolTypeKw).map(_ => BoolTypeStx)

  val nullType: Parser[TypeStx] =
    P(nullTypeKw).map(_ => NullTypeStx)

  val refType: Parser[TypeStx] =
    P(ident).map(RefTypeStx)

  val atomicType: Parser[TypeStx] =
    P(intType | dblType | strType | boolType | nullType | refType)

  // -----

  val stmt: Parser[StmtStx] =
    P(letStmt | typeStmt | exprStmt)

  // -----

  val letStmt: Parser[StmtStx] =
    P(letKw ~ ws ~/ ident ~ ws ~ (":" ~ ws ~ tpe ~ ws).? ~ "=" ~ ws ~/ expr)
      .map { case (name, tpe, expr) => LetStmtStx(name, tpe, expr) }

  val typeStmt: Parser[StmtStx] =
    P(letTypeKw ~ ws ~/ ident ~ ws ~ "=" ~ ws ~/ tpe)
      .map { case (name, tpe) => TypeStmtStx(name, tpe) }

  val exprStmt: Parser[StmtStx] =
    P(expr).map(ExprStmtStx)

  // -----

  val expr: Parser[ExprStx] =
    P(block)

  // -----

  def validateBlock(stmts: Seq[StmtStx]): Parser[ExprStx] =
    (stmts.init, stmts.last) match {
      case (stmts, last: ExprStmtStx) => Pass.map(_ => BlockExprStx(stmts.toList, last.expr))
      case (stmts, last: LetStmtStx)  => Fail
      case (stmts, last: TypeStmtStx) => Fail
    }

  val blockHit: Parser[ExprStx] =
    P(doKw ~ ws ~ stmt.rep(min = 1, sep = nl) ~ ws ~ endKw)
      .flatMap(validateBlock)

  val block: Parser[ExprStx] =
    P(blockHit | cond)

  // -----

  val condHit: Parser[ExprStx] =
    P(ifKw ~ ws ~ expr ~ ws ~ thenKw ~ ws ~ expr ~ ws ~ elseKw ~ ws ~ expr)
      .map { case (test, ifTrue, ifFalse) => CondExprStx(test, ifTrue, ifFalse) }

  val cond: Parser[ExprStx] =
    P(condHit | infix)

  // -----

  def createInfix(op: Parser[InfixOp], subExprStx: Parser[ExprStx]): Parser[ExprStx] =
    P(subExprStx ~ (ws ~ op ~ ws ~ subExprStx).rep).map {
      case (head, tail) =>
        tail.foldLeft(head) { (a, pair) =>
          val (op, b) = pair
          InfixExprStx(op, a, b)
        }
    }

  val infix: Parser[ExprStx] =
    P(infixOps.foldRight(cast)(createInfix))

  // -----

  val cast: Parser[ExprStx] =
    P(prefix ~ (ws ~ ":" ~ ws ~ tpe).rep)
      .map { case (head, tail) => tail.foldLeft(head)(CastExprStx) }

  // -----

  val prefixHit: Parser[ExprStx] =
    P(prefixOp ~ ws ~ prefix)
      .map { case (op, arg) => PrefixExprStx(op, arg) }

  val prefix: Parser[ExprStx] =
    P(prefixHit | select)

  // -----

  val select: Parser[ExprStx] =
    P(apply ~ (ws ~ "." ~ ws ~ ident).rep)
      .map { case (head, tail) => tail.foldLeft(head)(SelectExprStx) }

  // -----

  val applyHit: Parser[ExprStx] =
    P(ident ~ ws ~ "(" ~/ ws ~ expr.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")")
      .map { case (name, args) => AppExprStx(RefExprStx(name), args.toList) }

  val apply: Parser[ExprStx] =
    P(applyHit | func)

  // -----

  val funcArg: Parser[FuncArgStx] =
    P(ident ~ (ws ~ ":" ~ ws ~ tpe).?)
      .map { case (name, tpe) => FuncArgStx(name, tpe) }

  val parenFuncHit: Parser[ExprStx] =
    P(("<" ~ ws ~ ident.rep(sep = ws ~ ", " ~ ws) ~ ws ~ ">").? ~ "(" ~ ws ~ funcArg.rep(sep = ws ~ "," ~ ws) ~ ws ~ ")" ~ ws ~ (":" ~ ws ~ parenType ~ ws).? ~ "->" ~ ws ~/ expr)
      .map { case (typeArgs, args, rType, expr) => FuncExprStx(typeArgs.fold(List.empty[String])(_.toList), args.toList, rType, expr) }

  val noParenFuncHit: Parser[ExprStx] =
    P(ident ~ ws ~ "->" ~ ws ~/ expr)
      .map { case (name, expr) => FuncExprStx(Nil, List(FuncArgStx(name, None)), None, expr) }

  val func: Parser[ExprStx] =
    P(parenFuncHit | noParenFuncHit | obj)

  // -----

  val field: Parser[(String, ExprStx)] =
    P((ident | stringToken) ~ ws ~/ ":" ~ ws ~/ expr)

  val objHit: Parser[ExprStx] =
    P("{" ~ ws ~/ field.rep(sep = ws ~ "," ~ ws.~/) ~ ws ~ "}".~/)
      .map(_.toList)
      .map(ObjExprStx)

  val obj: Parser[ExprStx] =
    P(objHit | arr)

  // -----

  val arrHit: Parser[ExprStx] =
    P("[" ~ ws ~/ expr.rep(sep = ws ~ "," ~/ ws) ~ ws ~ "]".~/)
      .map(_.toList)
      .map(ArrExprStx)

  val arr: Parser[ExprStx] =
    P(arrHit | paren)

  // -----

  val parenHit: Parser[ExprStx] =
    P("(" ~ ws ~ expr ~ ws ~ ")").map(ParenExprStx)

  val paren: Parser[ExprStx] =
    P(parenHit | atom)

  // -----

  val atom: Parser[ExprStx] =
    P(str | double | int | trueExprStx | falseExprStx | nullExprStx | ref)

  val str: Parser[ExprStx] =
    P(stringToken).map(StrExprStx)

  val double: Parser[ExprStx] =
    P(doubleToken).map(_.toDouble).map(DblExprStx)

  val int: Parser[ExprStx] =
    P(intToken).map(_.toInt).map(IntExprStx)

  val trueExprStx: Parser[ExprStx] =
    P(trueKw).map(_ => BoolExprStx(true))

  val falseExprStx: Parser[ExprStx] =
    P(falseKw).map(_ => BoolExprStx(false))

  val nullExprStx: Parser[ExprStx] =
    P(nullKw).map(_ => NullExprStx)

  val ref: Parser[ExprStx] =
    P(ident.map(RefExprStx))
}
