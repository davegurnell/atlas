package atlas

import cats.Show
import cats.implicits._

sealed abstract class Expr extends Product with Serializable

final case class RefExpr(id: String) extends Expr
final case class LetExpr(varName: String, varType: Option[Type], expr: Expr) extends Expr

final case class AppExpr(func: Expr, args: List[Expr]) extends Expr
final case class InfixExpr(op: InfixOp, arg1: Expr, arg2: Expr) extends Expr
final case class PrefixExpr(op: PrefixOp, arg: Expr) extends Expr

final case class FuncExpr(args: List[FuncArg], resultType: Option[Type], body: Expr) extends Expr
final case class FuncArg(argName: String, argType: Option[Type] = None)

final case class BlockExpr(stmts: List[Expr], expr: Expr) extends Expr
// final case class SelectExpr(expr: Expr, field: String) extends Expr
final case class CondExpr(test: Expr, trueArm: Expr, falseArm: Expr) extends Expr
final case class CastExpr(expr: Expr, asType: Type) extends Expr

// final case class ObjExpr(fields: List[(String, Expr)]) extends Expr
final case class ArrExpr(exprs: List[Expr]) extends Expr
final case class StrExpr(value: String) extends Expr
final case class IntExpr(value: Int) extends Expr
final case class DblExpr(value: Double) extends Expr
final case class BoolExpr(value: Boolean) extends Expr
final case object NullExpr extends Expr

object Expr {
  implicit val argShow: Show[FuncArg] =
    Show.show { case FuncArg(a, t) => show"$a : $t" }

  implicit val show: Show[Expr] =
    Show.show {
      case RefExpr(id)        => s"$id"
      case LetExpr(n, t, e)   => show"let $n : $t = $e"
      case AppExpr(f, as)     => show"$f(${as.map(_.show).mkString(", ")})"
      case InfixExpr(o, a, b) => show"$a ${o.id} $b"
      case PrefixExpr(o, a)   => show"${o.id}$a"
      case FuncExpr(a, r, b)  => show"(${a.map(_.show).mkString(", ")}) : $r -> $b"
      case BlockExpr(s, e)    => show"do${s.map(_.show).mkString(" ", "; ", ";")} $e end"
      // case SelectExpr() =>
      case CondExpr(a, b, c)  => show"if $a then $b else $c"
      case CastExpr(a, t)     => show"$a : $t"
      // case ObjExpr(fs) =>
      case ArrExpr(as)        => show"[${as.map(_.show).mkString(" ", ", ", " ")}]"
      case StrExpr(v)         => show"'${v.replaceAll("'", "\'")}'"
      case IntExpr(v)         => v.toString
      case DblExpr(v)         => v.toString
      case BoolExpr(v)        => v.toString
      case NullExpr           => "null"
    }
}