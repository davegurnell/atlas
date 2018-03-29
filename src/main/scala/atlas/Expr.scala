package atlas

import cats.Show
import cats.implicits._

sealed abstract class Expr extends Product with Serializable

final case class RefExpr(id: String, loc: SourceLoc) extends Expr
final case class LetExpr(varName: String, expr: Expr, loc: SourceLoc) extends Expr

final case class AppExpr(func: Expr, args: List[Expr], loc: SourceLoc) extends Expr
final case class InfixExpr(op: InfixOp, arg1: Expr, arg2: Expr, loc: SourceLoc) extends Expr
final case class PrefixExpr(op: PrefixOp, arg: Expr, loc: SourceLoc) extends Expr

final case class FuncExpr(args: List[FuncArg], body: Expr, loc: SourceLoc) extends Expr
final case class FuncArg(argName: String)

final case class ProgExpr(stmts: List[Expr], expr: Expr, loc: SourceLoc) extends Expr
final case class BlockExpr(stmts: List[Expr], expr: Expr, loc: SourceLoc) extends Expr
final case class SelectExpr(expr: Expr, field: String, loc: SourceLoc) extends Expr
final case class CondExpr(test: Expr, trueArm: Expr, falseArm: Expr, loc: SourceLoc) extends Expr
final case class ParenExpr(expr: Expr, loc: SourceLoc) extends Expr

final case class ObjExpr(fields: List[(String, Expr)], loc: SourceLoc) extends Expr
final case class ArrExpr(exprs: List[Expr], loc: SourceLoc) extends Expr
final case class StrExpr(value: String, loc: SourceLoc) extends Expr
final case class IntExpr(value: Int, loc: SourceLoc) extends Expr
final case class DblExpr(value: Double, loc: SourceLoc) extends Expr
final case class BoolExpr(value: Boolean, loc: SourceLoc) extends Expr
final case class NullExpr(loc: SourceLoc) extends Expr

final case class SourceLoc(start: Int, end: Int)

object Expr {
  implicit val argShow: Show[FuncArg] =
    Show.show { case FuncArg(a) => show"$a" }

  implicit val show: Show[Expr] =
    Show.show {
      case RefExpr(id, _)        => id
      case LetExpr(n, e, _)      => show"let $n = $e"
      case AppExpr(f, as, _)     => show"$f(${as.map(_.show).mkString(", ")})"
      case InfixExpr(o, a, b, _) => show"$a ${o.id} $b"
      case PrefixExpr(o, a, _)   => show"${o.id}$a"
      case FuncExpr(a, b, _)     => show"(${a.map(_.show).mkString(", ")}) -> $b"
      case ProgExpr(s, e, _)     => show"${s.map(_.show).mkString(" ", "; ", ";")} $e"
      case BlockExpr(s, e, _)    => show"do${s.map(_.show).mkString(" ", "; ", ";")} $e end"
      case SelectExpr(a, b, _)   => show"$a.$b"
      case CondExpr(a, b, c, _)  => show"if $a then $b else $c"
      case ParenExpr(a, _)       => show"($a)"
      case ObjExpr(fs, _)        => show"{${fs.map { case (n, e) => show"$n: $e" }.mkString(" ", ", ", " ")}}"
      case ArrExpr(as, _)        => show"[${as.map(_.show).mkString(" ", ", ", " ")}]"
      case StrExpr(v, _)         => show"'${v.replaceAll("'", "\'")}'"
      case IntExpr(v, _)         => v.toString
      case DblExpr(v, _)         => v.toString
      case BoolExpr(v, _)        => v.toString
      case NullExpr(_)           => "null"
    }
}
