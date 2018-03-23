package atlas

import cats.Show
import cats.implicits._

sealed abstract class TExpr extends Product with Serializable {
  def tpe: TypeVar
}

final case class TRefExpr(tpe: TypeVar, id: String) extends TExpr
final case class TLetExpr(tpe: TypeVar, varName: String, varType: Option[Type], expr: TExpr) extends TExpr

final case class TAppExpr(tpe: TypeVar, func: TExpr, args: List[TExpr]) extends TExpr
final case class TInfixExpr(tpe: TypeVar, op: InfixOp, arg1: TExpr, arg2: TExpr) extends TExpr
final case class TPrefixExpr(tpe: TypeVar, op: PrefixOp, arg: TExpr) extends TExpr

final case class TFuncExpr(tpe: TypeVar, args: List[TFuncArg], resultType: Option[Type], body: TExpr) extends TExpr
final case class TFuncArg(tpe: TypeVar, argName: String, argType: Option[Type] = None)

final case class TBlockExpr(tpe: TypeVar, stmts: List[TExpr], expr: TExpr) extends TExpr
// final case class Select(tpe: TypeVar, expr: TExpr, field: String) extends TExpr
final case class TCondExpr(tpe: TypeVar, test: TExpr, trueArm: TExpr, falseArm: TExpr) extends TExpr
final case class TCastExpr(tpe: TypeVar, expr: TExpr, asType: Type) extends TExpr

// final case class TObj(tpe: TypeVar, fields: List[(String, TExpr)]) extends TExpr
final case class TArrExpr(tpe: TypeVar, exprs: List[TExpr]) extends TExpr
final case class TStrExpr(tpe: TypeVar, value: String) extends TExpr
final case class TIntExpr(tpe: TypeVar, value: Int) extends TExpr
final case class TDblExpr(tpe: TypeVar, value: Double) extends TExpr
final case class TBoolExpr(tpe: TypeVar, value: Boolean) extends TExpr
final case class TNullExpr(tpe: TypeVar) extends TExpr

object TExpr {
  implicit val argShow: Show[TFuncArg] =
    Show.show { case TFuncArg(tpe, a, t) => show"<$tpe>$a : $t" }

  implicit val show: Show[TExpr] =
    Show.show {
      case TRefExpr(tpe, id)        => show"<$tpe>$id"
      case TLetExpr(tpe, n, t, e)   => show"let <$tpe>$n : $t = $e"
      case TAppExpr(tpe, f, as)     => show"<$tpe>$f(${as.map(_.show).mkString(", ")})"
      case TInfixExpr(tpe, o, a, b) => show"<$tpe>($a ${o.id} $b)"
      case TPrefixExpr(tpe, o, a)   => show"<$tpe>${o.id}$a"
      case TFuncExpr(tpe, a, r, b)  => show"<$tpe>(${a.map(_.show).mkString(", ")}) : $r -> $b"
      case TBlockExpr(tpe, s, e)    => show"<$tpe>do${s.map(_.show).mkString(" ", "; ", ";")} $e end"
      // case TSelectExpr() =>
      case TCondExpr(tpe, a, b, c)  => show"<$tpe>(if $a then $b else $c)"
      case TCastExpr(tpe, a, t)     => show"<$tpe>($a : $t)"
      // case TObjExpr(fs) =>
      case TArrExpr(tpe, as)        => show"<$tpe>[${as.map(_.show).mkString(" ", ", ", " ")}]"
      case TStrExpr(tpe, v)         => show"<$tpe>'${v.replaceAll("'", "\'")}'"
      case TIntExpr(tpe, v)         => show"<$tpe>$v"
      case TDblExpr(tpe, v)         => show"<$tpe>$v"
      case TBoolExpr(tpe, v)        => show"<$tpe>$v"
      case TNullExpr(tpe)           => show"<$tpe>null"
    }
}